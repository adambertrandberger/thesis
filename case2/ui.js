const device1 = new Signal(),
      pullSignal = new Signal(),
      databaseSignal = new Signal(),
      channel = new Channel();

const deviceSF = new SF(async () => {
    return new Date();
});

const dataStore = [];
const databaseSF = new SF(async value => {
    await Promise.wait(100); // it takes 100ms to send to the database
    dataStore.push(value);
});

databaseSF.pullFrom(deviceSF, 100); // push data to the database every 100ms

// .run(...) must be called after all connections have been attached
databaseSF.run(databaseSignal); // attaches the database signal to the SF
deviceSF.run(device1);

