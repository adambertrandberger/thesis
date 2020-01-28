class Channel {
    constructor(init={}) {
        this.buffer = [];

        // bufferThreshold -- what value indicates that we are
        // receiving too much data in the buffer?
        this.bufferThreshold = init.bufferThreshold || 100;

        // the rate to check if the buffer has been filled
        // (if ~take~ was called when nothing was in the buffer)
        this.pollingRate = init.pollingRate || 100;

        this.onEmptyBuffer = init.onEmptyBuffer || (() => {});
        this.onFullBuffer = init.onFullBuffer || (() => {});
    }

    // adds a value to the channel
    put(value) {
        if (this.buffer.length > this.bufferThreshold) {
            this.onFullBuffer(this);
            return;
        }
        this.buffer.push(value);
    }

    // take -- get a value from the channel
    // should block until a value is there
    async take(block=True) {
        // if the channel is empty, receive will block
        // this isn't a great way of doing it (introduces latency)
        // but it works for a proof of concept
        if (block && this.buffer.length === 0) {
            this.onEmptyBuffer(this);
            await Promise.wait(this.pollingRate);
            return await this.take();
        }
        
        return this.buffer.shift();
    }
}

const ControlMessages = {
    adjustSampleRate: x => [],
};

class Signal {
    constructor() {
        this.controlChannel = new Channel();
        this.dataChannel = new Channel({
            onFullBuffer: () => {
                this.controlChannel.put();
            },
            onEmptyBuffer: () => {
                this.controlChannel.put();
            }
        });
    }

    // pushes data (given by ~getValue~) to the stream ~stream~ every
    // ~deltaTime~ milliseconds
    async pushTo(stream, getValue, deltaTime) {
        while (true) {
            await Promise.wait(deltaTime);
            this.dataChannel.put(getValue());
        }
    }
    
    // pulls data from the stream ~stream~ every ~deltaTime~ milliseconds
    async pullFrom(signal, deltaTime) {
        while (true) {
            await Promise.wait();

            // check if there are any control messages (don't block if there aren't any)
            const message = await signal.controlChannel.take(false);

            if (message) {
                
            }
            this.dataChannel.put(await signal.dataChannel.take());
        }
    }
}

class SignalFunction {
    constructor(signal) {
        this.signal = signal;
    }
}
