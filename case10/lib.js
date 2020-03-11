class Stream {
    constructor(update=(x => x), debug=false) {
        this.update = update;
        
        // buffer for storing all incoming (unprocessed) data
        this.buffer = new Buffer();
        
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
            console.log('Latency: ', this.buffer.averageLatency());
            console.log('Throughput (samples per second): ', this.buffer.averageThroughputPerSecond());            
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

    static loop() {
        const stream = new Stream(),
              intervals = new Set();
        const looper = async (delay=1000) => {
            console.log(delay);
            Array.from(intervals).map(clearInterval);
            intervals.add(setInterval(() => {
                stream.propagate(null);
            }, delay));
            return null;
        };
        stream.update = looper;
        return stream;
    }
}

// a buffer that keeps statistics on items that go in and out
class Buffer {
    constructor() {
        this.buffer = [];

        // how many items have passed thru the buffer
        this.totalItems = 0;

        // how long we have taken between queing and dequeing
        this.totalDuration = 0;

        this.startTime = new Date();
    }

    averageLatency() {
        if (this.totalItems > 0 && this.totalDuration > 0) {
            // how much time per item
            return this.totalDuration / this.totalItems;
        } else {
            return 0;
        }
    }

    averageThroughputPerSecond() {
        return this.totalItems/((new Date() - this.startTime)/1000);
    }

    que(value) {
        this.buffer.push({ value, time: new Date() });
    }

    deque() {
        const { value, time } = this.buffer.shift();
        this.totalDuration += (new Date() - time);
        this.totalItems += 1;
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

