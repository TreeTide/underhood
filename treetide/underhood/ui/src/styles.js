import jss from 'jss'
import preset from 'jss-preset-default'
import color from 'color'
 
// One time setup with default plugins and settings.
jss.setup(preset())
 
const styles = {
  button: {
    fontSize: 12,
    textDecoration: 'underline',
    '&:hover': {
      background: 'blue'
    }
  },
  ctaButton: {
    extend: 'button',
    '&:hover': {
      background: color('blue')
        .darken(0.3)
        .hex()
    }
  },
  '@media (min-width: 1024px)': {
    button: {
      width: 200
    }
  },
  layout: {
    position: 'absolute',
    top: 0,
    bottom: 0,
    left: 0,
    right: 0
  },
}
 
const {classes} = jss.createStyleSheet(styles).attach()

export default classes
 
