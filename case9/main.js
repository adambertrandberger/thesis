const looper = Stream.seq(loop(100),
                          async () => {
                              await Promise.wait(1000);
                              return Math.random();
                          },
                          async x => {
                              await Promise.wait(120);
                              // console.log(x);
                              return Stream.print();
                          });

const mouseMove =
      Stream.seq(fromEvent('mousemove'),
                 e => [e.x, e.y],
                 coords => {
                     if (coords[0] < 100) {
                         mouseMove.send('stop');
                         return Stream.stop();
                     }
                     return coords;
                 },
                 async x => {
                     await Promise.wait(120);
                     return Stream.print();
                 });

const replayExample = Stream.seq(x => x,
                                 x => {
                                     if (x < 9) {
                                         return Stream.replay(x + 1);
                                     } else {
                                         return x;
                                     }
                                 },
                                 x => console.log(x));
