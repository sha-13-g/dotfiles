# import catppuccin

config.load_autoconfig()


# catppuccin.setup(c, 'macchiato')

# Setting Dark Mode
# config.set("colors.webpage.darkmode.enabled", True)
# config.set("colors.webpage.preferred_color_scheme","dark")

# Auto Save Session (useful in Unpredictable Situations)
config.set("auto_save.session", True)

# Change Downloads tab position
config.set("downloads.position","bottom")

#Change the download directory
config.set("downloads.location.directory", "~/downloads/")

# Custom keybinds
config.bind('B', 'hint links spawn mpv {hint-url}')
config.bind('aY', 'open -t https://youtube.com/')
config.bind('ay', 'open https://youtube.com/')
config.bind('at', 'open https://twitch.tv/')
config.bind('aT', 'open -t https://twitch.tv/')
config.bind('ag', 'open https://github.com/')
config.bind('aG', 'open https://mail.google.com/mail/u/0/#inbox/')

#Create new window for each tab
config.set("tabs.tabs_are_windows", True)

# JS, cookies, encoding, headers, fonts, status-bar
c.content.autoplay = False
c.content.cookies.accept = "all"
c.content.default_encoding = "utf-8"
c.content.headers.user_agent = (
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko)"
    " Chrome/80.0.3987.163 Safari/537.36"
)
c.statusbar.widgets = ["keypress", "progress", "url", "scroll"]
c.content.javascript.can_access_clipboard = True

# Set Colors
palette = {
    'background': '#282a36',
    'background-alt': '#282a36',
    'background-attention': '#181920',
    'border': '#282a36',
    'current-line': '#44475a',
    'selection': '#44475a',
    'foreground': '#f8f8f2',
    'foreground-alt': '#e0e0e0',
    'foreground-attention': '#ffffff',
    'comment': '#6272a4',
    'cyan': '#8be9fd',
    'green': '#50fa7b',
    'orange': '#ffb86c',
    'pink': '#ff79c6',
    'purple': '#bd93f9',
    'red': '#ff5555',
    'yellow': '#f1fa8c'
}
spacing = {
    'vertical': 2,
    'horizontal': 2
}
padding = {
    'top': spacing['vertical'],
    'right': spacing['horizontal'],
    'bottom': spacing['vertical'],
    'left': spacing['horizontal']
}

# Background color of the completion widget category headers.
c.colors.completion.category.bg = palette['background']

# Bottom border color of the completion widget category headers.
c.colors.completion.category.border.bottom = palette['border']

# Top border color of the completion widget category headers.
c.colors.completion.category.border.top = palette['border']

# Foreground color of completion widget category headers.
c.colors.completion.category.fg = palette['foreground']

# Background color of the completion widget for even rows.
c.colors.completion.even.bg = palette['background']

# Background color of the completion widget for odd rows.
c.colors.completion.odd.bg = palette['background-alt']

# Text color of the completion widget.
c.colors.completion.fg = palette['foreground']

# Background color of the selected completion item.
c.colors.completion.item.selected.bg = palette['selection']

# Bottom border color of the selected completion item.
c.colors.completion.item.selected.border.bottom = palette['selection']

# Top border color of the completion widget category headers.
c.colors.completion.item.selected.border.top = palette['selection']

# Foreground color of the selected completion item.
c.colors.completion.item.selected.fg = palette['foreground']

# Foreground color of the matched text in the completion.
c.colors.completion.match.fg = palette['orange']

# Color of the scrollbar in completion view
c.colors.completion.scrollbar.bg = palette['background']

# Color of the scrollbar handle in completion view.
c.colors.completion.scrollbar.fg = palette['foreground']

# Background color for the download bar.
c.colors.downloads.bar.bg = palette['background']

# Background color for downloads with errors.
c.colors.downloads.error.bg = palette['background']

# Foreground color for downloads with errors.
c.colors.downloads.error.fg = palette['red']

# Color gradient stop for download backgrounds.
c.colors.downloads.stop.bg = palette['background']

# Color gradient interpolation system for download backgrounds.
# Type: ColorSystem
# Valid values:
#   - rgb: Interpolate in the RGB color system.
#   - hsv: Interpolate in the HSV color system.
#   - hsl: Interpolate in the HSL color system.
#   - none: Don't show a gradient.
c.colors.downloads.system.bg = 'none'

# Background color for hints. Note that you can use a `rgba(...)` value
# for transparency.
c.colors.hints.bg = palette['background']

# Font color for hints.
c.colors.hints.fg = palette['purple']

# Hints
c.hints.border = '1px solid ' + palette['background-alt']

# Font color for the matched part of hints.
c.colors.hints.match.fg = palette['foreground-alt']

# Background color of the keyhint widget.
c.colors.keyhint.bg = palette['background']

# Text color for the keyhint widget.
c.colors.keyhint.fg = palette['purple']

# Highlight color for keys to complete the current keychain.
c.colors.keyhint.suffix.fg = palette['selection']

# Background color of an error message.
c.colors.messages.error.bg = palette['background']

# Border color of an error message.
c.colors.messages.error.border = palette['background-alt']

# Foreground color of an error message.
c.colors.messages.error.fg = palette['red']

# Background color of an info message.
c.colors.messages.info.bg = palette['background']

# Border color of an info message.
c.colors.messages.info.border = palette['background-alt']

# Foreground color an info message.
c.colors.messages.info.fg = palette['comment']

# Background color of a warning message.
c.colors.messages.warning.bg = palette['background']

# Border color of a warning message.
c.colors.messages.warning.border = palette['background-alt']

# Foreground color a warning message.
c.colors.messages.warning.fg = palette['red']

# Background color for prompts.
c.colors.prompts.bg = palette['background']

# Border used around UI elements in prompts.
c.colors.prompts.border = '1px solid ' + palette['background-alt']

# Foreground color for prompts.
c.colors.prompts.fg = palette['cyan']

# Background color for the selected item in filename prompts.
c.colors.prompts.selected.bg = palette['selection']

# Background color of the statusbar in caret mode.
c.colors.statusbar.caret.bg = palette['background']

# Foreground color of the statusbar in caret mode.
c.colors.statusbar.caret.fg = palette['orange']

# Background color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.bg = palette['background']

# Foreground color of the statusbar in caret mode with a selection.
c.colors.statusbar.caret.selection.fg = palette['orange']

# Background color of the statusbar in command mode.
c.colors.statusbar.command.bg = palette['background']

# Foreground color of the statusbar in command mode.
c.colors.statusbar.command.fg = palette['pink']

# Background color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.bg = palette['background']

# Foreground color of the statusbar in private browsing + command mode.
c.colors.statusbar.command.private.fg = palette['foreground-alt']

# Background color of the statusbar in insert mode.
c.colors.statusbar.insert.bg = palette['background-attention']

# Foreground color of the statusbar in insert mode.
c.colors.statusbar.insert.fg = palette['foreground-attention']

# Background color of the statusbar.
c.colors.statusbar.normal.bg = palette['background']

# Foreground color of the statusbar.
c.colors.statusbar.normal.fg = palette['foreground']

# Background color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.bg = palette['background']

# Foreground color of the statusbar in passthrough mode.
c.colors.statusbar.passthrough.fg = palette['orange']

# Background color of the statusbar in private browsing mode.
c.colors.statusbar.private.bg = palette['background-alt']

# Foreground color of the statusbar in private browsing mode.
c.colors.statusbar.private.fg = palette['foreground-alt']

# Background color of the progress bar.
c.colors.statusbar.progress.bg = palette['background']

# Foreground color of the URL in the statusbar on error.
c.colors.statusbar.url.error.fg = palette['red']

# Default foreground color of the URL in the statusbar.
c.colors.statusbar.url.fg = palette['foreground']

# Foreground color of the URL in the statusbar for hovered links.
c.colors.statusbar.url.hover.fg = palette['cyan']

# Foreground color of the URL in the statusbar on successful load
c.colors.statusbar.url.success.http.fg = palette['green']

# Foreground color of the URL in the statusbar on successful load
c.colors.statusbar.url.success.https.fg = palette['green']

# Foreground color of the URL in the statusbar when there's a warning.
c.colors.statusbar.url.warn.fg = palette['yellow']

# Status bar padding
c.statusbar.padding = padding

# Background color of the tab bar.
# Type: QtColor
c.colors.tabs.bar.bg = palette['selection']

# Background color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.bg = palette['selection']

# Foreground color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.fg = palette['foreground']

# Color for the tab indicator on errors.
# Type: QtColor
c.colors.tabs.indicator.error = palette['red']

# Color gradient start for the tab indicator.
# Type: QtColor
c.colors.tabs.indicator.start = palette['orange']

# Color gradient end for the tab indicator.
# Type: QtColor
c.colors.tabs.indicator.stop = palette['green']

# Color gradient interpolation system for the tab indicator.
# Type: ColorSystem
# Valid values:
#   - rgb: Interpolate in the RGB color system.
#   - hsv: Interpolate in the HSV color system.
#   - hsl: Interpolate in the HSL color system.
#   - none: Don't show a gradient.
c.colors.tabs.indicator.system = 'none'

# Background color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.bg = palette['selection']

# Foreground color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.fg = palette['foreground']

# Background color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.bg = palette['background']

# Foreground color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.fg = palette['foreground']

# Background color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.bg = palette['background']

# Foreground color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.fg = palette['foreground']

# Tab padding
c.tabs.padding = padding
c.tabs.indicator.width = 1
c.tabs.favicons.scale = 1

c.url.searchengines = {
        'DEFAULT': 'https://duckduckgo.com/?q={}',
        'i': 'https://duckduckgo.com/?q={}&iar=images&iax=images&ia=images',
        'vic': 'https://la.wikipedia.org/w/index.php?search={}&title=Specialis%3AQuaerere',
        'red': 'https://reddit.com/r/{}',
        'tpb': 'http://thepiratebay.org/search/{}',
        'andr': 'https://developer.android.com/develop/index.html?q={}',
        'ghr': 'https://github.com/search?utf8=%E2%9C%93&q={}&type=',
        'ghn': 'https://github.com/search?utf8=%E2%9C%93&q={}+in%3Aname&type=Repositories',
        'ud': 'https://www.urbandictionary.com/define.php?term={}&utm_source=search-action',
        'ddg': 'https://duckduckgo.com/?q={}&t=ha&iar=images',
        'aw': 'https://wiki.archlinux.org/index.php?title=Special%3ASearch&search={}',
        'yt': 'https://www.youtube.com/results?search_query={}',
        'tw': 'https://https://www.twitch.tv/search?term={}',
        'go': 'https://www.google.com/search?q={}',
        'w': 'https://www.wikipedia.org/search-redirect.php?family=wikipedia&language=en&search={}&language=en&go=Go',
        'sk': 'https://www.skytorrents.in/search/all/ed/1/?l=en-us&q={}',
        'whl': 'https://alpha.wallhaven.cc/search?q={}&categories=111&purity=100&sorting=views&order=desc',
        'whh': 'https://alpha.wallhaven.cc/search?q={}&categories=111&purity=100&atleast=2560x1440&sorting=views&order=desc&page=2',
        }
