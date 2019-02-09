export default {
	start(app) {
		customElements.define('auction-api-copy-text', class extends HTMLElement {
			constructor() {
				super()
				this._timeout = null
				this._onButtonClick = this._onButtonClick.bind(this)
			}

			connectedCallback() {
				this._note = this.querySelector('auction-api-copy-text-note')
				this._button = this.querySelector('button')
				this._button.addEventListener('click', this._onButtonClick)
			}

			disconnectedCallback() {
				clearTimeout(this._timeout)
			}

			_onButtonClick() {
				const textArea = document.createElement('textarea')
				textArea.innerHTML = this.getAttribute('data-copy-text')
				this.appendChild(textArea)
				textArea.select()

				let successful = false
				try {
					successful = document.execCommand('copy')
				} catch (error) {
					successful = false
				}

				this.removeChild(textArea)

				if (successful) {
					this._note.textContent = 'クリップボードにコピーしました'
				} else {
					this._note.textContent = '⌘C to Copy'
				}

				clearTimeout(this._timeout)
				this._timeout = setTimeout(() => {
					this._note.style.visibility = 'hidden'
				}, 3000)
				this._note.style.visibility = 'visible'
			}
		})
	}
}
