import { PolymerElement } from '@polymer/polymer/polymer-element.js';
import '@polymer/paper-button/paper-button.js';
import './tearound-tickbox.js';
import { html } from '@polymer/polymer/lib/utils/html-tag.js';
class TeaRoundLoggedIn extends PolymerElement {
  static get template() {
    return html`
    <style>
      .roommembers {
        display: inline-block;
        width: 200px;
        margin: 10px;
      }

      paper-button {
        display: inline-block;
        margin: 10px;
        width: 600px;

        background-color: lightgray;
        border: 1px solid darkgray;
        border-radius: 10px;
        text-align: center;
        vertical-align: middle;
        padding: 200px 0;
      }

      .bold {
        font-weight: bold;
      }

      .right {
        float: right;
        margin: 0 40% 0 0;
      }
    </style>
    <div class="roommembers">
      <template is="dom-repeat" items="[[_members]]">
        <div>
          <tearound-tickbox ticked="[[item.inround]]"></tearound-tickbox>
          <span>[[item.name]]</span>
        </div>
      </template>
    </div>

    <template is="dom-if" if="[[timeleft]]">
      <div class="right">Timeleft: [[timeleft]] seconds</div>
    </template>

    <paper-button on-tap="changeWantTea">[[buttonmessage]]</paper-button>


    <template is="dom-if" if="[[teamaker]]">
      <ul>
        [[teamaker.name]] will be making the drinks for:-

        <template is="dom-repeat" items="[[teafor]]">
          <li>[[item.name]]</li>
        </template>
      </ul>
    </template>
`;
  }

  static get is() {
    return 'tearound-loggedin'
  }
  static get properties() {
    return {
      members: {
        type: Array,
        observer: 'membersChanged',
        value: []
      },
      _members: {
        type: Array
      },
      wantteamembers: {
        type: Array,
        observer: 'membersChanged'
      },
      inround: Boolean,
      buttonmessage: {
        type: String,
        value: "Time for tea"
      },
      ourid: {
        type: String,
        value: ''
      },
      teamaker: {
        type: Object,
        value: null
      },
      teafor: {
        type: Array,
        value: []
      },
      timeleft: {
        type: Number,
        value: 0
      }
    }
  }

  _log(newValue, oldValue) {
    console.log("Updating from ", oldValue, newValue)
  }

  _wantTeaMember(id) {
    return !!this.wantteamembers.find((item) => {
      return item.id == id
    })
  }

  membersChanged() {
    this.inround = this._wantTeaMember(this.ourid)
    this.buttonmessage = this.inround ? "No thanks" : "Time for tea"
    var processedMembers = []
    for (let loop = 0; loop < this.members.length; loop += 1) {
      var item = Object.assign({}, this.members[loop])
      item.inround = this._wantTeaMember(item.id)
      processedMembers[loop] = item
    }
    this._members = processedMembers;
  }

  changeWantTea() {
    let eventName = this.inround ? "notea" : "wanttea"
    this.dispatchEvent(new CustomEvent(eventName, {}))
  }
}
window.customElements.define(TeaRoundLoggedIn.is, TeaRoundLoggedIn);
