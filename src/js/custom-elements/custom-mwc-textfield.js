import { TextField } from '@material/mwc-textfield';

class CustomTextField extends TextField {
  static get observedAttributes() {
    return [...TextField.observedAttributes, 'unit'];
  }

  delete() {
    const deleteEvent = new CustomEvent('MDCTextfield:delete');
    this.dispatchEvent(deleteEvent);

    this.value = '';
  }

  createUnitContainer() {
    // Unit container can be only one
    if (!this.shadowRoot.querySelector('.mdc-text-field__unit')) {
      const field = this.shadowRoot.querySelector('.mdc-text-field');
      const input = this.shadowRoot.querySelector('.mdc-text-field__input');

      // Get initial input padding right
      const inputPaddingRight = parseInt(
        window.getComputedStyle(input, null).getPropertyValue('padding-right')
      );

      // Create container for unit value
      const unit = document.createElement('div');
      unit.className = 'mdc-text-field__unit';
      unit.style.cssText =
        `
        position: absolute;
        top: 0;
        right: ${inputPaddingRight}px;
        bottom: 0;
        display: flex;
        align-items: center;
      ` /* Copied from mdc-text-field__input to match text style and alignment */ +
        `
        padding: 12px 6px 14px 8px;
        font-family: Roboto, sans-serif;
        -webkit-font-smoothing: antialiased;
        font-size: 1rem;
        font-weight: 400;
        letter-spacing: 0.009375em;
        color: var(--mdc-text-field-ink-color, rgba(0, 0, 0, 0.87));
      `;
      unit.innerText = this.getAttribute('unit');

      // Append unit container to dom
      field.appendChild(unit);

      // Set initial input padding considering unit container width
      input.style.paddingRight = `${parseInt(inputPaddingRight) +
        unit.offsetWidth}px`;
    }
  }

  updateUnitValue(value) {
    const input = this.shadowRoot.querySelector('.mdc-text-field__input');
    const unit = this.shadowRoot.querySelector('.mdc-text-field__unit');

    if (input && unit) {
      const inputPaddingRight = parseInt(
        window.getComputedStyle(input, null).getPropertyValue('padding-right')
      );

      const plainInputPaddingRight = inputPaddingRight - unit.offsetWidth;

      unit.innerText = value;

      input.style.paddingRight = `${plainInputPaddingRight +
        unit.offsetWidth}px`;
    }
  }

  connectedCallback() {
    super.connectedCallback();

    const deleteCallback = e => {
      if (e.composedPath()[0].classList.contains('mdc-text-field__icon')) {
        this.delete();

        this.reportValidity();
      }
    };

    this.shadowRoot.addEventListener('click', deleteCallback);
    this.shadowRoot.addEventListener('touchstart', deleteCallback);

    this.validityTransform = (value, _) => {
      const isValid =
        !value.length || new RegExp(this.getAttribute('regex')).test(value);

      const validatedEvent = new CustomEvent('MDCTextfield:validated', {
        detail: isValid,
      });
      this.dispatchEvent(validatedEvent);

      return {
        valid: isValid,
        patternMismatch: !isValid,
      };
    };
  }

  firstUpdated() {
    super.firstUpdated();

    const icon = this.shadowRoot.querySelector('.mdc-text-field__icon');
    const style_ = document.createElement('style');

    // Some fixes/rewrites for component styling as mwc is still in beta
    style_.innerHTML = `
      .mdc-text-field__input {
        text-align: right !important;
      }

      ${
        icon && icon.innerText === 'delete'
          ? `.mdc-text-field__icon {
            cursor: pointer !important;
            pointer-events: all !important;
          }`
          : ''
      }
    `;

    this.shadowRoot.appendChild(style_);

    if (this.getAttribute('unit')) {
      // FIXME: Crossbrowser hotfix =)
      setTimeout(() => this.createUnitContainer(), 0);
    }
  }

  attributeChangedCallback(name, oldValue, newValue) {
    super.attributeChangedCallback(name, oldValue, newValue);

    if (name === 'unit') {
      this.updateUnitValue(newValue);
    }

    if (name === 'value' && oldValue !== newValue) {
      !this._validity.valid && this.reportValidity();
    }
  }
}

window.customElements.define('custom-mwc-textfield', CustomTextField);
