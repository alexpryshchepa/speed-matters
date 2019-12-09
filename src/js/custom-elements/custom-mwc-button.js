import { Button } from '@material/mwc-button';

class CustomButton extends Button {
  firstUpdated() {
    super.firstUpdated();

    const button = this.shadowRoot.querySelector('.mdc-button');

    if (button !== null) {
      button.style.cssText = `
        height: auto;
        min-height: 36px;
      `;
    }
  }
}

window.customElements.define('custom-mwc-button', CustomButton);
