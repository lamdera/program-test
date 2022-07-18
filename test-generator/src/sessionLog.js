let sessionLog =
    { initialWindowSize: { width: window.innerWidth, height: window.innerHeight }
    , initialUrl: window.location.href
    , events: []
    };

function getClick(node) {
    let hasClickEvent = false;
    if (node.elmFs) {
        if (node.elmFs.click) {
            hasClickEvent = true;
        }
    }

    if (node.tagName == "A") {
        let href = node.attributes.getNamedItem("href");
        if (href) {
            return { args: [ href.value ], tag: "clickedLink" };
        }
        return null;
    }
    else if (node.id !== "") {
        if (node.tagName == "INPUT") {
            return { args: [ node.id ], tag: "clickedInput" };
        }
        else if (hasClickEvent) {
            return { args: [ node.id ], tag: "clickedButton" };
        }
    }
    else if (hasClickEvent) {
        return null;
    }
    else if (node.parentNode) {
        return getClick(node.parentNode);
    }
    return null;
}

document.documentElement.addEventListener(
    'click',
    (event) => {
        let click = getClick(event.target);
        if (click) {
            sessionLog.events.push(click);
            console.log(sessionLog);
        }
    },
    true);

document.body.addEventListener(
    'keydown',
    (event) => {
        if (document.activeElement) {
            console.log(document.activeElement);
            if (document.activeElement.id !== "") {
                sessionLog.events.push({ tag: "typedInput", args: [ document.activeElement.id, event.key ] });
                console.log(sessionLog);
            }
        }
    });
