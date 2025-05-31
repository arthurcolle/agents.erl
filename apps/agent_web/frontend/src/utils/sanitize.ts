// HTML sanitization utility to prevent XSS attacks

const ALLOWED_TAGS = [
  'p', 'br', 'strong', 'em', 'u', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6',
  'blockquote', 'code', 'pre', 'ol', 'ul', 'li', 'a', 'img', 'span', 'div'
]

const ALLOWED_ATTRIBUTES = {
  'a': ['href', 'title', 'target'],
  'img': ['src', 'alt', 'title', 'width', 'height'],
  'code': ['class'],
  'pre': ['class'],
  'span': ['class'],
  'div': ['class']
}

const ALLOWED_PROTOCOLS = ['http:', 'https:', 'mailto:']

export function sanitizeHtml(html: string): string {
  // Create a temporary DOM element to parse HTML
  const temp = document.createElement('div')
  temp.innerHTML = html
  
  // Recursively clean the DOM tree
  cleanNode(temp)
  
  return temp.innerHTML
}

function cleanNode(node: Node) {
  // Process child nodes first (bottom-up to avoid issues with node removal)
  const children = Array.from(node.childNodes)
  children.forEach(child => cleanNode(child))
  
  if (node.nodeType === Node.ELEMENT_NODE) {
    const element = node as Element
    const tagName = element.tagName.toLowerCase()
    
    // Remove disallowed tags
    if (!ALLOWED_TAGS.includes(tagName)) {
      // Move children up to parent before removing the element
      while (element.firstChild) {
        element.parentNode?.insertBefore(element.firstChild, element)
      }
      element.remove()
      return
    }
    
    // Clean attributes
    const attributes = Array.from(element.attributes)
    attributes.forEach(attr => {
      const attrName = attr.name.toLowerCase()
      const allowedAttrs = ALLOWED_ATTRIBUTES[tagName] || []
      
      // Remove disallowed attributes
      if (!allowedAttrs.includes(attrName)) {
        element.removeAttribute(attr.name)
        return
      }
      
      // Sanitize attribute values
      if (attrName === 'href' || attrName === 'src') {
        const url = attr.value.trim()
        if (url.startsWith('javascript:') || url.startsWith('data:')) {
          element.removeAttribute(attr.name)
          return
        }
        
        // Check protocol for absolute URLs
        try {
          const urlObj = new URL(url, window.location.href)
          if (!ALLOWED_PROTOCOLS.includes(urlObj.protocol)) {
            element.removeAttribute(attr.name)
          }
        } catch {
          // Invalid URL, remove the attribute
          element.removeAttribute(attr.name)
        }
      }
    })
    
    // Add rel="noopener noreferrer" to external links
    if (tagName === 'a' && element.getAttribute('target') === '_blank') {
      element.setAttribute('rel', 'noopener noreferrer')
    }
  }
}

// Sanitize CSS to prevent style-based attacks
export function sanitizeCss(css: string): string {
  // Remove any script-related CSS
  css = css.replace(/javascript:/gi, '')
  css = css.replace(/expression\s*\(/gi, '')
  css = css.replace(/@import/gi, '')
  css = css.replace(/behavior:/gi, '')
  css = css.replace(/-moz-binding:/gi, '')
  
  return css
}

// Escape HTML special characters for safe text display
export function escapeHtml(text: string): string {
  const div = document.createElement('div')
  div.textContent = text
  return div.innerHTML
}