class ControlMessage {
    static DecreaseDeltaTime(amount) {
        return new ControlMessage('SampleRate', -amount);
    }

    static IncreaseDeltaTime(amount) {
        return new ControlMessage('SampleRate', amount);
    }
    
    constructor(property, amount=0) {
        this.property = property;
        this.amount = amount;
    }
}

class Channel {
    constructor(init={}) {
        this.dataBuffer = [];
        this.controlBuffer = [];        

        // bufferThreshold -- what value indicates that we are
        // receiving too much data in the buffer?
        this.dataBufferThreshold = init.dataBufferThreshold || 10;

        // the rate to check if the buffer has been filled
        // (if ~take~ was called when nothing was in the buffer)
        this.pollingRate = init.pollingRate || 100;

        this.onBufferEmpty = init.onBufferEmpty || (x => x);
        this.onBufferFull = init.onBufferFull || (x => x);        
    }

    // adds a value to the channel
    put(value) {
        if (this.dataBuffer.length >= this.dataBufferThreshold) {
            this.onBufferFull(this);            
        }
        this.dataBuffer.push(value);
    }

    // take -- get a value from the channel
    // should block until a value is there
    async take(block=true) {
        // if the channel is empty, receive will block
        // this isn't a great way of doing it (introduces latency)
        // but it works for a proof of concept

        if (this.dataBuffer.length === 0) {
            this.onBufferEmpty(this);
            
            if (block) {
                await Promise.wait(this.pollingRate);
                return await this.take();
            } else {
                return null;
            }
        }

        return this.dataBuffer.shift();
    }
}

class ManagedChannel {
    constructor(getSampleRate) {
        this.percentage = 0.1; // the percent to lower/raise the sampling rate
        this.dataChannel = new Channel({
            onBufferEmpty: () => {
                this.controlChannel.put(ControlMessage.DecreaseDeltaTime(getSampleRate() * this.percentage));
            },
            onBufferFull: () => {
                this.controlChannel.put(ControlMessage.IncreaseDeltaTime(getSampleRate() * this.percentage));
            }
        });
        this.controlChannel = new Channel();
    }

    put(value) {
        this.dataChannel.put(value);
    }

    async take(block=true) {
        return await this.dataChannel.take();
    }
}

class Process {
    constructor(f) {
        this.f = f;
    }

    run() {
        this.f(...arguments);
    }
}

class Sampler {
    static New(deltaTime=100, f) {
        const sampler = new Sampler(deltaTime, f);
        sampler.managedChannel = new ManagedChannel(() => sampler.deltaTime);
        return [sampler, sampler.managedChannel];
    }
    
    constructor(deltaTime=100, f) { // should be private
        this.deltaTime = deltaTime;
        this.f = f;
    }

    async resolveMessages() {
        let message = await this.managedChannel.controlChannel.take();
        while (message) {
            // for now, always accept requests to alter the sample rate
            if (message.property === 'SampleRate') {
                this.deltaTime += message.amount;
                console.log('Modified the sample rate by ' + message.amount.toFixed(5) + '. New sample rate is ' + this.deltaTime);
            }
            
            message = await this.managedChannel.controlChannel.take();
        }
    }

    async run() {
        while (true) {
            await this.f(this.managedChannel);
            this.resolveMessages();
            await Promise.wait(this.deltaTime);
        }
    }
}

