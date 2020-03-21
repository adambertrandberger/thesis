const database = [],
      visualization = new TimeWindow(2000),
      deviceDriver = Stream.loop(1000),
      rand = new Stream(async () => {
          return Math.random();
      }), // fake signal data
      process = new Stream(async x => {
          // a fake KPI calculation that waits 500ms and negates the argument
          await wait(500);
          updateDOMList('processing', process.buffer.window.getValues());
          return -x;
      }, false),
      store = new Stream(async x => {
          // fake database storage which always takes 500ms
          await wait(500);
          database.push(x);
          updateDOMList('database', database);
      }, false),
      visualizeDriver = Stream.loop(100),
      visualize = new Stream(async () => {
          let val = database.shift();
          visualization.add(val);
          updateDOMList('visualization', visualization.getValues());
      });

// setup data collection and analysis graph:
deviceDriver.to(rand);
rand.to(process);
process.to(store);
// start the loop
deviceDriver.run();

// setup data visualization 
visualizeDriver.to(visualize);
// start the loop
visualizeDriver.run();

// The meta-program that alters the DFG while it is running the program above
const loop = Stream.loop(1000),
      matchVisualizationToDatabase = link(visualizeDriver, visualize,
                                          deviceDriver, store),
      matchDatabaseToVisualization = link(deviceDriver, store,
                                          visualizeDriver, visualize),
      controller = new Controller(deviceDriver);

loop.to(matchVisualizationToDatabase);
// start the meta-program
loop.run();

function updateDOMList(name, list) {
    window[name].textContent = '[' + list.map(x => x === null || x === undefined ? 'null' : x.toFixed(3)) + ']';     
}

function updateDOM() {
    window['rand-throughput'].textContent = rand.buffer.getThroughput();
    window['process-throughput'].textContent = process.buffer.getThroughput();
    window['store-throughput'].textContent = store.buffer.getThroughput();
    window['visualize-throughput'].textContent = visualize.buffer.getThroughput();
    
    window['visualization-driver-rate'].textContent = visualizeDriver.delay;
    window['device-driver-rate'].textContent = deviceDriver.delay;
}

function rampUp(loop) {
    loop.delay += 100;
    updateDOM();
}

function rampDown(loop) {
    loop.delay -= 100;
    updateDOM();
}

setInterval(updateDOM, 100);

// if the slave's throughput drops below the master's throughput,
// increase the slave's sampling rate.
// if the slave's throguhput is above the master's throughput,
// decrease the slave's sampling rate.
function link(slaveLoop, slave, masterLoop, master) {
    return new Stream(() => {
        if (slave.getThroughput() > master.getThroughput()) {
            slaveLoop.delay += 100;
        } else if (slave.getThroughput() < master.getThroughput()) {
            slaveLoop.delay = Math.max(slaveLoop.delay - 100, 0);
        }
    });
}
