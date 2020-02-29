class Propagator {
    constructor(graph) {
        this.streams = [];
        this.load(graph);
    }

    newStream(update=(x => x)) {
        const stream = new Stream(update);
        this.streams.push(stream);
        return stream;
    }

    switchTo(name) {
        return {
            name: name,
            type: 'switch'
        };
    }

    getStream(name) {
        return this.graph[name][0];
    }

    getConnections(name) {
        return this.graph[name][1] || [];
    }

    // a stream requesting a propagation of a certain value
    request(streamName, value, c) { // continuation?
        const stream = this.getStream(streamName),
              newValue = stream.update(value);

        // if we are switching
        // this kind of switching replaces the current node with the 'switchTo' node
        // it is different than a goto because you lose all variables on the stack
        if (typeof newValue === 'object' &&  newValue.type === 'switch')  {
            this.request(newValue.name, value);
        }
        
        for (const sinkName of this.getConnections(streamName)) {
            this.request(sinkName, newValue);
        }
    }

    // loads a dataflow graph into the propagator
    load(o) {
        this.graph = o;
        for (const name in o) {
            let value = o[name],
                expr = value[0];
            
            if (typeof expr !== 'function') {
                expr = x => x;
            }

            o[name][0] = this.newStream(expr);
        }

        for (const name in o) {
            const value = o[name],
                  stream = value[0],
                  connections = value[1] || [];

            for (const connection of connections) {
                stream.connect(o[connection][0]);
            }
        }
    }
}

class Stream {
    constructor(update=(x=>x)) {
        this.update = update;
        this.sources = [];
        this.sinks = [];
    }

    connect(stream) {
        this.sinks.push(stream);
        stream.sources.push(this);
    }
}

class Channel {
    constructor(from, to) {
        this.from = from;
        this.to = to;
        this.buffer = [];
    }

    send(value) {
        this.buffer.push(value);
    }
}
