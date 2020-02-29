class Stream {
    constructor(update=(x=>x)) {
        this.update = update;
        this.sources = [];
        this.sinks = [];
    }

    propagate(value) {
        value = this.update(value);
        for (const sink of this.sinks) {
            sink.propagate(value);
        }
    }

    connect(stream) {
        this.sinks.push(stream);
        stream.sources.push(this);
    }
}

const source = new Stream(),
      addTwo = new Stream(x => x + 2),
      subtractThree = new Stream(x => x - 3),
      print = new Stream(x => console.log(x));



source.connect(addTwo);
source.connect(subtractThree);
addTwo.connect(print);
subtractThree.connect(print);

/* This is the graph this constructs:
 *
 *      Source
 *      /   \
 *     /     \
 *  addTwo  subThree
 *    \      /
 *     \    /
 *      print
 *
 */
