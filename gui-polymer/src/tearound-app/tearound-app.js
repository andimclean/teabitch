import {
  PolymerElement
} from '@polymer/polymer/polymer-element.js';
import {
  html
} from '@polymer/polymer/lib/utils/html-tag.js';
import '@polymer/app-route/app-route.js';
import '@polymer/app-route/app-location.js';
import '@polymer/iron-pages/iron-pages.js';
import './elements/tearound-login.js';
import './elements/tearound-loggedin.js';

import 'socket.io-client/dist/socket.io.js';
/**
 * @customElement
 * @polymer
 */
class TearoundApp extends PolymerElement {
  static get template() {
    return html `
    <style>
      :host {
        display: block;
      }

      .banner {
        pointer-events: auto;
        display: flex;
        flex-direction: row;
        justify-content: space-between;
        flex-shrink: 0;
        padding: 20px 80px;
        position: relative;
        left: 0px;
        top: 0px;
        box-sizing: border-box;
        background-color: #ababab;
      }

      .logo {
        display: block;
        flex-shrink: 0;
        width: 60px;
        height: 60px;
        position: relative;
        left: 0px;
        top: 0px;
        box-sizing: border-box;
      }
    </style>
    <div class="banner">
      <img class="logo" alt="" src="src/img/logo.png">
      <div class="el none__3402792823 " style="pointer-events: auto; display: flex; flex-direction: row; flex-shrink: 1; position: relative; left: 0px; top: 0px; box-sizing: border-box;">
        <div class="el " style="pointer-events: none; display: flex; flex-direction: row; align-items: flex-end; margin: -10px; flex-shrink: 1; width: calc(100% + 20px); align-self: flex-end; position: relative; left: 0px; top: 0px; box-sizing: border-box;"></div>
      </div>      
    </div>
    <app-location route="{{ourRoute}}"></app-location>
    <app-route route="{{ourRoute}}" pattern="/:version/:room" data="{{roomData}}" tail="{{roomTail}}" active="{{roomActive}}"></app-route>
    <app-route route="{{roomTail}}" pattern="/:name" data="{{userData}}" tail="{{userTail}}" active="{{userActive}}"></app-route>

    <iron-pages attr-for-selected="dataselect" selected-attribute="andi" fallback-selection="connecting" selected="{{pageOn}}">
      <div dataselect="connecting">Connecting...</div>
      <div dataselect="loggingin">Asking to join room
        <b>[[roomData.room]]</b>
      </div>
      <tearound-login dataselect="login" name="{{_ourName}}" room="{{_ourRoom}}" version="[[_ourVersion]]" on-joinroom="joinRoom"></tearound-login>
      <tearound-loggedin dataselect="loggedin" ourid="[[_ourID]]" members="[[members]]" wantteamembers="[[wantteamembers]]" teamaker="[[teamaker]]" teafor="[[teafor]]" on-notea="_noTea" timeleft="[[_timeleft]]" on-wanttea="_wantTea"></tearound-loggedin>
    </iron-pages>
    `
  }
  static get is() {
    return 'tearound-app';
  }
  static get properties() {
    return {
      ourRoute: String,
      roomData: Object,
      roomTail: Object,
      roomActive: {
        type: Boolean,
        value: false,
        observer: 'roomChanged'
      },
      userData: Object,
      userTail: Object,
      userActive: {
        type: Boolean,
        value: false,
        observer: 'roomChanged'
      },

      connected: {
        type: Boolean,
        value: false,
        observer: 'roomChanged'
      },
      _inroom: {
        type: Boolean,
        value: false,
        observer: 'roomChanged'
      },
      pageOn: String,
      inRound: {
        type: Boolean,
        value: false
      },
      _socket: Object,
      _ourID: String,
      _ourName: String,
      _ourRoom: String,
      _ourVersion: String,

      wantteamembers: {
        type: Array,
        value: [],
        notify: true
      },

      members: {
        type: Array,
        value: [],
        notify: true
      },
      timeleft: {
        type: Number,
        value: 0,
        observer: 'updateTimeleft'
      },
      _timeleft: {
        type: Number,
        computed: '_woutoutTimeLeft(timeleft)'
      }
    }
  }

  ready() {
    super.ready()
    this._socket = io('localhost:3000')

    this._socket.on('connect', () => {
      this.connected = true
    })

    this._socket.on('joined', (data) => {
      this.members = data.members
      this._inroom = true
      this.pageOn = 'loggedin'
    })

    this._socket.on('yourid', (data) => {
      this._ourID = data.id
    })

    this._socket.on('roundstarted', (data) => {
      this.timeleft = data.timeleft
      //todo add notification
    })

    this._socket.on('inround', (data) => {
      this.timeleft = data.timeleft || 0
      this.maxTime = data.timeleft || 0
      this.wantteamembers = data.wantingtea || []
    })

    this._socket.on('roundcomplete', (data) => {
      this.teamaker = data.teamaker
      this.teafor = data.teafor
      this.wantteamembers = []
      this.timeleft = 0;
    })

    this._socket.on('reconnecting', () => {
      this.connected = true
    })
  }

  _woutoutTimeLeft() {
    return Math.floor(this.timeleft / 1000)
  }
  roomChanged() {
    if (!this.connected) {
      this.pageOn = 'connecting'
    } else {
      this._ourName = this.userData.name
      this._ourRoom = this.roomData.room
      this._ourVersion = this.roomData.version || 'polymer'
      if (this._ourName && this._ourRoom) {
        this.pageOn = 'loggingin'
        if (!this._inroom) {
          this._joinRoom(this.roomData.room, this.userData.name)
        } else {
          this.pageOn = 'loggedin'
        }
      } else {
        this.pageOn = 'login'
      }
    }
  }

  _wantTea() {
    this._socket.emit("wanttea")
  }

  _noTea() {
    this._socket.emit("notea")
  }

  joinRoom(event) {
    window.history.pushState({}, null,
      `/${event.detail.version}/${event.detail.roomname}/${event.detail.membername}`)
    window.dispatchEvent(new CustomEvent('location-changed'));
  }
  _joinRoom(roomName, memberName) {
    if (this._socket) {
      this._socket.emit('join', {
        roomname: this.roomData.room,
        membername: this.userData.name
      })
    }
  }

  updateTimeleft() {
    if (this.timeleft > 0) {
      setTimeout(() => {
        this.timeleft -= this.timeleft > 0 ? 1000 : 0
      }, 1000)
    }
  }
}

window.customElements.define(TearoundApp.is, TearoundApp);