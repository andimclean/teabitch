require('./css/index.scss')
const Clipboard = require('clipboard')

const socket = require('socket.io-client')

const Elm = require('./Main.elm')

const app = Elm.Main.embed(document.getElementById('main'))

var askNotification = true

function getPosition(el) {
    var xPos = 0;
    var yPos = 0;

    while (el) {
        if (el.tagName == "BODY") {
            // deal with browser quirks with body/window/document and page scroll
            var xScroll = el.scrollLeft || document.documentElement.scrollLeft;
            var yScroll = el.scrollTop || document.documentElement.scrollTop;

            xPos += (el.offsetLeft - xScroll + el.clientLeft);
            yPos += (el.offsetTop - yScroll + el.clientTop);
        } else {
            // for all other non-BODY elements
            xPos += (el.offsetLeft - el.scrollLeft + el.clientLeft);
            yPos += (el.offsetTop - el.scrollTop + el.clientTop);
        }

        el = el.offsetParent;
    }
    return {
        x: xPos,
        y: yPos
    };
}

var clipboard = new Clipboard('.copy-button', {
    text: function(trigger) {
        var s = trigger.getAttribute("data-clipboard-text");
        var t = s.substring(1,s.length - 1)        
        return t;
    }
})
clipboard.on('success', (e) => {
    var element = e.trigger
    var elementPos = getPosition(element)
    var tooltip = document.createElement("div")
    tooltip.classList = "tooltip"
    tooltip.innerText = "Copied to clipboard";
    tooltip.style.position = "fixed"
    tooltip.style.left = (elementPos.x + 10 ) + 'px'
    tooltip.style.top = (elementPos.y + 10 ) + 'px'
    document.body.appendChild(tooltip);

    setTimeout(() => {
        tooltip.remove()
    } , 4000)

})

const current = socket()
current.on('connect', () => {
    app.ports.connect.send(null)
})
current.on('joined', (data) => {
    app.ports.joined.send(data.members)
})

current.on('yourid', (data) => {
    app.ports.yourid.send(data)
})

current.on('roundstarted', (data) => {
    app.ports.roundstarted.send(data)
})

current.on('inround', (data) => {
    app.ports.wantingtea.send(data)
})

current.on('roundcomplete', (data) => {
    app.ports.roundcomplete.send(data)
})

current.on('reconnecting', () => {
    app.ports.disconnect.send(null)
})

app.ports.join.subscribe((person) => {
    current.emit("join", person)
})

app.ports.wantTea.subscribe((_) => {
    current.emit("wanttea")
})

app.ports.notea.subscribe((_) => {
    current.emit("notea")
})
app.ports.notify.subscribe((info) => {
    if ("Notification" in window) {
        if (askNotification && Notification.permission != "granted") {
            Notification.requestPermission((_permission) => {})
            askNotification = false;
        }
        var notification = new Notification(info.message, {
            icon: "/img/logo.png"
        })
        if (info.onclick) {
            notification.onclick = function () {
                app.ports.notificatinClicked.send(null)
                notification.close()
            }
        }
    }
})