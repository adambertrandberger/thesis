const database = [],
      visualization = new TimeWindow(2000),
      deviceDriver = Stream.loop(1000),
      rand = new Stream(async () => {
          return Math.random();
      }), // fake signal data
      process = new Stream(async x => {
          // a fake KPI calculation that waits 500ms and negates the argument
          await wait(500);
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
// start the loop with 100ms delay
deviceDriver.run();

// setup data visualization 
visualizeDriver.to(visualize);
// start the loop with 100ms delay
visualizeDriver.run();

// The meta-program that alters the DFG while it is running the program above
const loop = Stream.loop(1000),
      updateDFG = new Stream(() => {
          updateDOM();
          if (visualize.buffer.getThroughput() > store.buffer.getThroughput()) {
              visualizeDriver.delay = visualizeDriver.delay + 100;
              console.log('Increased delay of visualization to ' + visualizeDriver.delay);
          } else if (visualize.buffer.getThroughput() < store.buffer.getThroughput()) {
              visualizeDriver.delay = visualizeDriver.delay - 100;
              console.log('Decreased delay of visualization to ' + visualizeDriver.delay);
          }
      }),
      controller = new Controller(deviceDriver);

loop.to(updateDFG);
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
