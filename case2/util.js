Promise.wait = function (ms) {
    return new Promise(r => setTimeout(r, ms));
};

const Random = {
    // returns a random number in range (lo, hi)
    range: (lo, hi) => {
        const range = hi - lo + 1;
        return Math.floor(lo + Math.random()*range);
    }
};
