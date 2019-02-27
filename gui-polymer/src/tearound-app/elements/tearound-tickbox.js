import { PolymerElement } from '@polymer/polymer/polymer-element.js';
import { html } from '@polymer/polymer/lib/utils/html-tag.js';
class TearoundTickbox extends PolymerElement {
  static get template() {
    return html`
    <style>
      :host {
        width: 24px;
        height: 24px;
        display: inline-block;
      }

      .material-icons {
        font-family: 'Material Icons';
        font-weight: normal;
        font-style: normal;
        font-size: 24px;
        line-height: 1;
        letter-spacing: normal;
        text-transform: none;
        display: inline-block;
        white-space: nowrap;
        word-wrap: normal;
        direction: ltr;
        font-feature-settings: 'liga';
        -webkit-font-feature-settings: 'liga';
        -webkit-font-smoothing: antialiased;
        color: lightgreen;
      }
    </style>
    <template is="dom-if" if="[[ticked]]">
      <i class="material-icons">check_box</i>
    </template>
    <template is="dom-if" if="[[!ticked]]">
      <i class="material-icons">check_box_outline_blank</i>
    </template>
`;
  }

  static get is() {
    return 'tearound-tickbox'
  }
  static get properties() {
    return {
      ticked: {
        type: Boolean,
        value: true,
        reflectToAttribute: true,
        notify: true
      }
    }
  }
}
window.customElements.define(TearoundTickbox.is, TearoundTickbox);
