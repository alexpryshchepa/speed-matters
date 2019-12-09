import { Snackbar } from '@material/mwc-snackbar';

class CustomSnackbar extends Snackbar {
  static get observedAttributes() {
    return [...Snackbar.observedAttributes, 'open'];
  }
  
  attributeChangedCallback(name, oldValue, newValue) {
    super.attributeChangedCallback(name, oldValue, newValue);

    if (name === 'open') {
      if (newValue === 'true') {
        this.open();
      } else {
        this.close();
      }
    }

    if (
      name === 'labeltext'
      && this.getAttribute('open') === 'true'
    ) {
      this.open();
    }
  }
}

window.customElements.define('custom-mwc-snackbar', CustomSnackbar);
