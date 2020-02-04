import { Button } from '@material/mwc-button';

class CustomButton extends Button {
  firstUpdated() {
    super.firstUpdated();

    const style_ = document.createElement('style');

    // Some fixes for component styling as mwc is still in beta
    style_.innerHTML = `
      .mdc-button {
        height: auto !important;
        min-height: 36px !important;
      }

      .mdc-button--raised:not(:disabled),
      .mdc-button--unelevated:not(:disabled) {
        background-color: var(--mdc-theme-primary) !important;
      }

      .mdc-button .mdc-button__ripple::before,
      .mdc-button .mdc-button__ripple::after {
        background-color: var(--mdc-theme-primary) !important;
      }

      .mdc-button__label {
        white-space: nowrap !important;
      }
    `;

    this.shadowRoot.appendChild(style_);
  }
}

window.customElements.define('custom-mwc-button', CustomButton);
