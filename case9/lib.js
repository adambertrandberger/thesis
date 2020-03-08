/*
 * There are buffered streams for data, and normal Flapjax style propagation for events attached to them
 */

class Stream {
    static stop() {
        return new Stop();
    }
    static print() {
        return new Print();
    }
    static replay(x) {
        return new Replay(x);
    }

    static seq(...streams) {
        streams = streams.map(Stream.lift);
        if (streams.length === 1) {
            return streams[0];
        }
        for (let i=0; i<streams.length-1; ++i) {
            streams[i].then(streams[i + 1]);
        }
        return streams[0];
    }

    static lift(x) {
        if (x instanceof Stream) {
            return x;
        } else if (typeof x === 'function') {
            return new Stream({
                update: x
            });
        } else {
            return new Stream({
                update: () => x
            });
        }
    }
    
    
    constructor(handlers) {
        this.handlers = handlers;
        this.sinks = [];
        this.sources = [];
    }

    then(stream) {
        const from = this,
              to = stream;
        from.sinks.push(to);
        to.sources.push(from);
    }

    // triggers an event with this name, and value
    async send(name, value) {
        const b = Behavior.lift(value),
              handler = this.handlers[name] || (e => e),
              { time:latestTime, value:latestValue } = b.latest();
        value = await handler.bind(this)(latestValue);

        // write down history of which event was recieved from which stream and the value the behavior had
        b.update({ stream: this, event: name, value: value });

        if (value instanceof ControlEvent) {
            if (value instanceof Print) {
                console.log(b);
            } else if (value instanceof Stop){
                // do nothing
            } else if (value instanceof Replay) {
                const replay = value;
                const overrideValue = replay.value;
                const { stream, event, value } = b.earliest();
                stream.send(event, overrideValue === undefined ? value : overrideValue);
            }
        } else { // propagate
            const streams = 'push' ? this.sinks : this.sources;
            streams.map(s => s.send(name, value));
        }
    }
}
const buffer = (stream, eventName='update') => {
    let flushers = Promise.resolve(),
        buffer = [];
    const bufferedStream = new Stream({
        clear: async function () {
            buffer = [];
            return Stream.stop();
        },
    });
    bufferedStream[eventName] = async function (x) {
        if (x instanceof Behavior) {
            console.log(new Date() - x.at);
            return await stream.handlers[eventName](x.data);
        } else {
            buffer.push(new Behavior(x));
            flushers = flushers.then(async () => {
                while (buffer.length > 0) {
                    const x = buffer.shift();
                    await this.send(eventName, x);
                }
            });
            return Stream.stop();
        }
    };
    return bufferedStream;
};

class Behavior {
    constructor(value) {
        this.revisions = [new Revision(value)];
    }
    
    static lift(data) {
        if (data instanceof Behavior) {
            return data;
        } else {
            return new Behavior(data);
        }
    }

    update(value) {
        this.revisions.push(new Revision(value));
    }

    // binary search thru revisions
    at(time=new Date(), interpolate=(x => x)) {
        // TODO
    }

    latest() {
        if (this.revisions.length > 0) {
            return this.revisions[this.revisions.length - 1];
        } else {
            return null;
        }
    }

    earliest() {
        return this.revisions[0].value;
    }

    // how long has this behavior been alive for
    deltaT() {
        return this.reviisions[this.revisions.length - 1].time - this.revisions[0].time;
    }
}
class Revision {
    constructor(value, time=new Date()) {
        this.value = value;
        this.time = time;
    }
}

const loop = (ms=100, eventName='update') => {
    const timeoutIds = new Set();
    return new Stream({
        start: async function () {
            await this.send('stop');
            const loop = () => {
                const timeoutId = setTimeout(() => {
                    this.send(eventName);
                    loop();
                    timeoutIds.delete(timeoutId);
                }, ms);
                timeoutIds.add(timeoutId);
            };
            loop();
        },
        stop: function () {
            for (const id of timeoutIds) {
                clearTimeout(id);
            }
        }
    });
};

const fromEvent = (name) => {
    function eventListener (e) {
        stream.send('update', e);
    }
    const stream = new Stream({
        start: () => {
            addEventListener(name, eventListener);
        },
        stop: () => {
            removeEventListener(name, eventListener);
        }
    });
    return stream;
};

Promise.wait = function (ms) {
    return new Promise(resolve => {
        setTimeout(() => {
            resolve();
        }, ms);
    });
};

// Magic value to stop propagation
class ControlEvent {}
class Stop extends ControlEvent {}
class Print extends ControlEvent {}
class Replay extends ControlEvent {
    constructor(value) {
        super();
        this.value = value;
    }
}
