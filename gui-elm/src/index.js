require('./css/index.scss')
const socket = require('socket.io-client')

const Elm = require('./Main.elm')

const app = Elm.Main.embed(document.getElementById('main'))

const socketUrl = "http://192.168.86.34:3000/"

var askNotification = true

const current = socket(socketUrl)
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
            icon: "img/logo.png"
        })
        if (info.onclick) {
            notification.onclick = function () {
                app.ports.notificatinClicked.send(null)
                notification.close()
            }
        }
    }
})

app.ports.console.subscribe((message) => {
    console.log(message)
})