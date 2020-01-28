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
        this.onPut = init.onPut || (() => {});        
    }

    // adds a value to the channel
    put(value) {
        if (this.buffer.length >= this.bufferThreshold) {
            this.onFullBuffer(this);
            return;
        }
        this.onPut(value);
        this.buffer.push(value);
    }

    // take -- get a value from the channel
    // should block until a value is there
    async take(block=true) {
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

class Signal {
    constructor() {
        this.controlChannel = new Channel();
        this.dataChannel = new Channel({
            onFullBuffer: () => {
                //                this.controlChannel.put();
                console.log('full');
            },
            onEmptyBuffer: () => {
                //                this.controlChannel.put();
                console.log('empty');
            }
        });
    }
}

class SF {
    constructor(f=(x => x)) {
        this.f = f;
        
        this.fs = []; // push and pull functions
    }
    
    // pushes data (given by ~getValue~) to the stream ~stream~ every
    // ~deltaTime~ milliseconds
    pushTo(sf, deltaTime) {
        this.fs.push(async signal => {
            while (true) {
                await Promise.wait(deltaTime);
                signal.dataChannel.put(this.f());
            }
        });
    }
    
    // pulls data from the stream ~stream~ every ~deltaTime~ milliseconds
    pullFrom(sf, deltaTime) {
        this.fs.push(async signal => {
            while (true) {
                await Promise.wait();

                // check if there are any control messages (don't block if there aren't any)
                const controlMessage = await signal.controlChannel.take(false);

                if (controlMessage) {
                    
                }
                
                signal.dataChannel.put(this.f(await signal.dataChannel.take()));
            }
        });
    }

    run(signal) {
        for (const f of this.fs) {
            f(signal);
        }
    }
}


class ControlMessage {
    static DecreaseSampleRate(amount) {
        return new ControlMessage('SampleRate', -amount);
    }

    static IncreaseSampleRate(amount) {
        return new ControlMessage('SampleRate', amount);
    }
    
    constructor(property, amount=0) {
        this.property = property;
        this.amount = amount;
    }
}
