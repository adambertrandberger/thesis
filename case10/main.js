// A program that samples some signal 2 times per second
// and sends it to some "database".
// The database has a 1 second latency for every request
// This results in 1 sample of back pressure per second
const sample = Stream.loop(),
      rand = new Stream(() => Math.random()),
      store = new Stream(async x => {
          await wait(1000);
          console.log('Sent to the database: ', x);
      }, true);

sample.to(rand);
rand.to(store);
// this is how you start a Stream.loop loop:
sample.run(100); 

// The meta-program that alters the DFG while it is running the program above
const loop = Stream.loop(),
      updateDFG = new Stream(() => {
          controller.breadthFirst(stream => {
              const latency = stream.buffer.averageLatency();
              if (latency > 1000) {
                  // latency is too high, must lower sample rate
                  sample.run(500); // "sample" takes the sampleRate
                  console.log('Decreased sample rate to 500');
              }
          });
      }),
      controller = new Controller(sample);

loop.to(updateDFG);
loop.run(1000);
