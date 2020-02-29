Network(signalHz =>
        Quality(Bandwidth, signalHz/2),
        
        Source('Voltage',
               Parameter('sampleRate', 1000,
                         Increases, Bandwidth),
               DoInterval('sampleRate', () => {
                   out(new Date());
               })),
        
        Process('Batcher',
                Parameter('batchSize', 100,
                          Increases, Latency,
                          Increases, Bandwidth, false),
                DoForever(async () => {
                    const buffer = [];
                    while (buffer.length < batchSize) {
                        buffer.push(await in());
                    }
                    out(buffer);
                })),
        
        Sink('Database',
             DoForever(() => {
                 sendHTTP(await in());
             })));

new Network(signalHz =>
        Quality(Bandwidth, signalHz/2),
        
        Source('Voltage',
               Parameter('sampleRate', 1000,
                         Increases, Bandwidth),
               DoInterval('sampleRate', () => {
                   out(new Date());
               })),
        
        Process('Batcher',
                Parameter('batchSize', 100,
                          Increases, Latency,
                          Increases, Bandwidth, false),
                DoForever(async () => {
                    const buffer = [];
                    while (buffer.length < batchSize) {
                        buffer.push(await in());
                    }
                    out(buffer);
                })),
        
        Sink('Database',
             DoForever(() => {
                 sendHTTP(await in());
             })));


