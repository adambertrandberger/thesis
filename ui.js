function toggleBackPressure() {
    if (enableResolution) {
        event.target.textContent = 'No';
    } else {
        event.target.textContent = 'Yes';
    }
    enableResolution = !enableResolution;
}

function setBufferSizeDisplay(buffer) {
    window.bufferSize.textContent = buffer.length;
}

function deleteDots() {
    let dots = document.getElementsByClassName('dot');
    for (const dot of dots) {
        dot.remove();
    }

    if(document.getElementsByClassName('dot').length > 0) {
        deleteDots(); // if some were added during the delete
    }
}
