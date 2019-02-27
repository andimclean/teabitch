import { PolymerElement } from '@polymer/polymer/polymer-element.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import { html } from '@polymer/polymer/lib/utils/html-tag.js';
class Tearoun_Login extends PolymerElement {
  static get template() {
    return html`
    <style>
      .loginbox {
        vertical-align: top;
        display: inline-block;
        width: 200px;
        margin: 10px;
      }

      .blurb {
        display: inline-block;
        margin: 10px;
        max-width: 800px;
      }

      .bold {
        font-weight: bold;
      }

      paper-button {
        float: right;

        --paper-button: {
          background-color: var(--call-to-action, #44baff);
          color: white;
          font-weight: bold;
        }
      }
    </style>
    <div class="loginbox">
      <paper-input value="{{room}}" label="Room Name"></paper-input>
      <paper-input value="{{name}}" label="Your Name"></paper-input>
      <paper-button disabled\$="{{_disabled}}" on-tap="_join">Join Room</paper-button>
    </div>
    <div class="blurb">
      <h1 class="bold">Who's tea round is it? </h1>
      <p>Have you ever worked in an office where everyone is suppose to take turns in making the drinks?</p>
      <p>Are you scared seeing if anyone else wants a drink as it'll mean you will have to make everyone drinks again?
        So you don't ask and slowly dehydrate</p>
      <h2 class="bold">Then Tea Round is for you</h2>
      <p>Just login with your name and a unique name for the room. Then click on <span class="bold">Share</span> In the
        top right corner, which will copy your room's address to the clipboard, you can then paste this to the rest of
        your office workers (Vie email, Snapchat, Skype etc)")
      </p>
      <p>When it's time for a drink, just Hit <span class="bold">Time for tea</span> and everyone in the room will be
        notified it's time for a drink.
        Everyone who wants a drink clicks <span class="bold">Time for tea </span>Once the timer reaches zero, One of
        the people who want tea will be randomaly selected to go make it.</p>

      <p>This is a public server with no actual logins. So make your room name random.</p>
    </div>
`;
  }

  static get is() {
    return 'tearound-login'
  }
  static get properties() {
    return {
      name: {
        type: String,
        observer: "checkDisabled"
      },
      room: {
        type: String,
        observer: "checkDisabled"
      },
      version: String,
      _disabled: Boolean
    }
  }

  checkDisabled() {
    this._disabled = !(this.name && this.room)
  }

  _activeChanged(newValue, oldValue) {
    console.log("Valuechanged from ", oldValue, "to", newValue)
  }
  _join() {
    this.dispatchEvent(new CustomEvent("joinroom", {
      detail: {
        version: this.version,
        roomname: this.room,
        membername: this.name
      }
    }))
  }
}
window.customElements.define(Tearoun_Login.is, Tearoun_Login);
