Promise.wait = function (ms) {
    return new Promise(r => setTimeout(r, ms));
};
