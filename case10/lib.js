class Stream {
    constructor(update=(x => x), debug=false) {
        this.update = update;
        
        // buffer for storing all incoming (unprocessed) data
        this.buffer = new Buffer(1000);
        
        this.sources = [];
        this.sinks = [];

        // synchronize flushing of the buffer
        this.flushers = Promise.resolve();

        this.debug = debug;

        this.flushing = false;
    }

    to(stream) {
        this.sinks.push(stream);
        stream.sources.push(this);
    }

    push(value) {
        if (this.debug) {
            //            console.log('Throughput (samples per second): ', this.buffer.averageThroughputPerSecond());            
            console.log('Buffer: ', this.buffer.buffer);
        }
        this.buffer.que(value);
        if (!this.flushing) {
            this.flushing = true;
            this.flush();
        }
    }

    async run(value) {
        value = await this.update(value);
        await this.propagate(value);
    }

    // sends the data (that has already been processed by this node) to the connected sinks
    async propagate(value) {
        for (const sink of this.sinks) {
            sink.push(value);
        }
    }

    async flush() {
        if (this.debug) {
            console.log('FLUSSHING');
        }
        while (!this.buffer.empty()) {
            let value = this.buffer.deque();
            await this.run(value);
        }
        this.flushing = false;
    }

    static loop(delay=1000) {
        const stream = new Stream(),
              intervals = new Set();
        const looper = async () => {
            setTimeout(() => {
                stream.propagate(null);
                looper();
            }, stream.delay);
            return null;
        };
        stream.update = looper;
        stream.delay = delay;
        return stream;
    }
}

// A value that resets 
class TimeWindow {
    constructor(duration) {
        this.duration = duration;
        this.values = [];
    }

    add(value, time=new Date()) {
        this.value = value;
        this.values.push({ value, time });
        this.updateValues();
    }

    updateValues() {
        this.values = this.values.filter(x => new Date() - x.time < this.duration);
    }

    getValueCount() {
        return this.getValues().length;
    }

    getValues() {
        this.updateValues();
        return this.values.map(x => x.value);
    }
}

// a buffer that keeps statistics on items that go in and out
class Buffer {
    constructor(windowDuration) {
        this.buffer = [];

        // how many items have passed thru the buffer
        this.window = new TimeWindow(windowDuration);

        this.startTime = new Date();
    }

    getThroughput() {
        return this.window.getValueCount();
    }

    que(value) {
        this.buffer.push({ value, time: new Date() });
    }

    deque() {
        const { value, time } = this.buffer.shift();
        this.window.add(value);
        return value;
    }

    empty() {
        return this.buffer.length === 0;
    }
}

class Controller {
    constructor(root) {
        this.root = root;
    }

    breadthFirst(visit) {
        let frontier = [this.root],
            seen = new Set();
        while (frontier.length > 0) {
            frontier.map(s => {
                if (!seen.has(s)) {
                    visit(s);
                    seen.add(s);
                }
            });
            frontier = frontier
                .flatMap(s => s.sources.concat(s.sinks))
                .filter(s => !seen.has(s));
        }
    }
}

const wait = function (ms) {
    return new Promise(resolve => {
        setTimeout(() => {
            resolve();
        }, ms);
    });
};

