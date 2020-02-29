const p = new Propagator({
    source: [null, ['addTwo']],
    addTwo: [x => x + 2, ['subThree']],
    subThree: [x => x - 3, ['checkError']],
    checkError: [x => {
        if (x < 0) {
            return p.switchTo('error');
        }
        return p.switchTo('print');
    }],
    print: [x => console.log(x)],
    error: [x => console.error(x)]
});

p.request('source', 0);

