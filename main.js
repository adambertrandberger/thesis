let debug = true,
    enableResolution = true;

class SF { // StreamFunction
    constructor(f=(async x => x), sfs=[]) {
        this.f = f;
        this.sfs = sfs; // this is for using combinators over several stream functions at the same time like .all([...]).sum()
    }

    static all(sfs) {
        return new SF(undefined, sfs);
    }

    then(sf) {
        return new SF(async x => await this.f(x).then(async x => await sf.f(x)),
                      this.sfs.map(sf1 => sf1.then(sf)));
    }
    
    all(sfs) {
        return SF.all(sfs.map(sf => this.then(sf)));
    }

    repeat(n) {
        let sf = this;
        for (let i=0; i<n; ++i) {
            sf = sf.then(this);
        }
        return sf;
    }

    sum() { // assuming this SF has inner sfs and they are numbers
        return new SF(async x => {
            return this.apply(x).reduce((a, b) => a + b);
        });
    }

    join() { 
        return new SF(async x => {
            return this.apply(x);
        });
    }

    async apply(x) { // forces the SF to give a value for input "x"
        if (this.sfs.length > 0) {
            return await Promise.all(this.sfs.map(sf => sf.apply(x)));            
        } else {
            return await this.f(x);
        }
    }

    run(producer, interval=10) {
        // create a new consumer, that runs the signal function
        const consumer = new Stream({
            consume: async x => {
                await this.apply(x);
            }
        });

        if (debug) {
            window.consumer = consumer;
        }
        
        producer.listen(consumer);
        
        setInterval(() => {
            producer.emit();
        }, interval);
    }
}

class Stream {
    constructor(init={}) {
        this.buffer = [];
        this.consumers = []; // other streams who are listening to this stream
        this.consume = init.consume;

        this.totalConsumptionDuration = 0; // how much time (seconds) has been spent consuming
        this.consumptionCount = 0;

        this.windowStartTime = performance.now(); // the time the stream became alive
        this.totalInputs = 0; // how many data points have we received since the stream became alive?

        this.windowStartingSecond = this.getCurrentSecond(); // this is to track how long ago the window started -- this can probably be removed
        this.consumptionsDuringWindow = 0;

        this.windowSize = 0.7; //  window for throttling and for computing the average input

        this.flushing = false;
    }

    getCurrentSecond() {
        return +(performance.now() / 1000).toFixed();
    }

    isWithinWindow() { // is the current time within the latest window?
        return this.getCurrentSecond() - this.windowStartingSecond < this.windowSize;
    }

    async doConsumption(datum) {
        if (this.hasBackPressure() && enableResolution) {
            console.info('Skipping to prevent back pressure');
            return;
        }

        const before = performance.now();

        await this.consume(datum);
        
        if (this.isWithinWindow()) {
            this.consumptionsDuringWindow += 1;
        } else { // reset all statistics (so they don't get stale)
            this.windowStartingSecond = this.getCurrentSecond();
            this.consumptionsDuringWindow = 1;
        }
        
        this.consumptionCount += 1;
        this.totalConsumptionDuration += ((performance.now() - before)/1000);

        if (debug) {
            if (this.hasBackPressure()) {
                console.warn('Back pressure detected.\n\n' +
                             'Stream is consuming ' + this.averageInputPerWindow() + ' inputs per window, and can only process each sample at ' + this.averageConsumptionDuration() + ' seconds.\n\n');
                
            }
            // else {
            //     console.info('Consumption successful (no back pressure). Input per second is ' + this.averageInputPerWindow() + ' and processing each sample at ' + this.averageConsumptionDuration() + ' seconds.');
            // }
        }

    }

    averageConsumptionDuration() { // how long does consuming one datum from the buffer take?
        return this.totalConsumptionDuration / this.consumptionCount;
    }

    averageInputPerWindow() { // how much input is this stream pushed per second?
        const now = performance.now();
        const duration = ((now - this.windowStartTime)/1000);
        if (duration < 0.5) { // if the duration isn't long enough the average is going to be very wrong given 1 input in 0.1 seconds it implies that there are 1/0.01 (100) inputs per second
            return 0;
        }
        return (this.totalInputs / duration)*this.windowSize;
    }

    maximumConsumptionsForWindow() { // how many consumptions can we can do for the given window size
        return this.windowSize / this.averageConsumptionDuration();
    }

    hasBackPressure() {
        // we make a guess based on the average, to see if we will get back pressure
        const predictedBackPressure = (this.averageConsumptionDuration() * this.averageInputPerWindow()) > this.windowSize;
        const haveReachedMaximum = this.totalInputs >= this.maximumConsumptionsForWindow();
        return haveReachedMaximum && predictedBackPressure;
    }

    emit() { // removes an item from the buffer -- emits data from the buffer
        while (this.buffer.length > 0) { // empty entire buffer to all streams listening to this stream
            const datum = this.buffer.shift();
            for (const consumer of this.consumers) {
                consumer.receive(datum); // don't need to await this because we don't care about a return value
            }
        }
    }

    // Should be called when the stream receives new data
    // This can happen for both producers and consumers
    // For producers it happens when a new event 
    async receive(datum) {
        this.buffer.push(datum);
        this.totalInputs += 1;
        this.flush();
    }

    listen(otherStream) { // sets another stream to be a consumer on this stream (that means this is a producer, and they are a consumer)
        this.consumers.push(otherStream);
    }

    async flush() { // consume all of the data in the buffer
        if (this.flushing) {
            return;
        }
        
        this.flushing = true;

        // if we were provided a consume function, and there's data,
        while (typeof this.consume === 'function' &&  this.buffer.length > 0) {
            setBufferSizeDisplay(this.buffer);
            const datum = this.buffer.shift();
            await this.doConsumption(datum);
        }
        setBufferSizeDisplay(this.buffer);        
        
        // restart the windowStartTime and totalInputs so we do a moving average instead of all time
        if (performance.now() - this.windowStartTime > (this.windowSize*1000)) {
            this.windowStartTime = performance.now();
            this.totalInputs = 0;
        }

        this.flushing = false;
    }
}

const mouseMove = new Stream();
document.addEventListener('mousemove', e => {
    mouseMove.receive(e);
});

const addThree = new SF(async x => x + 3), // Int -> Int
      addTwo = new SF(async x => x + 2),
      addFive = addThree.then(addTwo),
      getScreenX = new SF(async x => x.screenX),
      catL = str => new SF(async x => str + x),
      catR = str => new SF(async x => x + str),      
      getAttr = name => new SF(async x => x[name]),
      getCoords = SF.all([getAttr('clientX'), getAttr('clientY')]).join(),
      print = new SF(async x => console.log(x)),
      wait = (n=1000) => new SF(async x => {
          // await 0.1 second
          await new Promise(resolve => {
              setTimeout(() => resolve(), n);
          });
          
          return x;
      });

// const printX = getAttr('screenX').then(catL('Screen X: ')).then(print),
//       printY = getAttr('screenY').then(catL('Screen Y: ')).then(print);

// printX.run(mouseMove);
// printY.run(mouseMove);

getCoords.then(wait(40)).then(new SF(async coords => {
    const span = document.createElement('span');
    span.className = 'dot';
    span.style.left = coords[0] + 'px';
    span.style.top = coords[1] + 'px';
    document.body.appendChild(span);
})).run(mouseMove);


//getCoords.then(print).run(mouseMove);
