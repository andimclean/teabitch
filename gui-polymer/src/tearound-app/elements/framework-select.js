import {
  PolymerElement
} from '@polymer/polymer/polymer-element.js';
import {
  html
} from '@polymer/polymer/lib/utils/html-tag.js';
import '@polymer/iron-ajax';
class FrameworkSelector extends PolymerElement {
  static get template() {
    return html `
    <iron-ajax auto="" url="/frameworks" handle-as="json" last-response="{{frameworks}}">
    </iron-ajax>
    <select>
      <template is="dom-repeat" items="{{frameworks}}">
        <option>item</option>
      </template>
    </select>`;
  }

  static get is() {
    return 'framework-selector';
  }
  static get properties() {}
}

window.customElements.define(FrameworkSelector.is, FrameworkSelector);