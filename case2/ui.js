// mimics blob storage -- everything in this list has made it from the device to blob storage
const dataStore = [];

// mimics a API call that sends data to blob storage, where the network always
// takes 200ms to send a sample
const databaseProcess = async channel => {
    while (true) {
        const value = await channel.take();
        await Promise.wait(200); // it takes 100ms over the network to send to the database
        console.log('Database: Received signal value=' + value);
        dataStore.push(value);
    }
};

// creates a new Sampler process, and also gives the managedChannel that is used by that process
const [device1, managedChannel] = Sampler.New(100, async () => {
    const data = new Date().getTime(); // pretend we are sending some signal value
    console.log('Device: Sent new signal value=' + data);
    managedChannel.put(data);
});

// start sending sampels to the managed channel:
device1.run();

// start ingesting data from the device
databaseProcess(managedChannel);
