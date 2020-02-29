class Metric {
    constructor() {

    }
}

const Bandwidth = new Metric(),
      Latency = new Metric();

class Network {
    constructor(...exprs) {
        
    }
}

class Process {
    constructor() {
        this.in = new Channel();
        this.out = new Channel();
    }
}

class Sink extends Process {
    constructor() {

    }
}

class Source extends Process {
    cosntructor() {
        
    }
}

class Parameter {
    constructor() {

    }
}

class Effect {}
const Increases = new Effect(),
      Decreases = new Effect();

class Relationship {
    constructor(effect, metric, certain=true) {
        this.effect = effect;
        this.metric = metric;
        this.certain = certain;
    }
}
