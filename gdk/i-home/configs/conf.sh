#video_pref="248+bestaudio/best"
video_pref="[height<=1080]"
#scrape 1 video link per channel instead of the default 2
sub_link_count=1
show_thumbnails=0
##}}}

external_menu () {
    #use rofi instead of dmenu
#    rofi -dmenu -i -theme-str '@import "menu.rasi" window {height:60%;width: 70%;}' -p "$1"
    # rofi -dmenu -i -config ~/.config/rofi/list.rasi -p "Search for :->"
    wofi -dib -W 90% -H 80% 
#      bemenu -i -l 20 --fn "ComicCodeLigatures Medium 20"
#    dmenu -i -l 20
}

thumbnail_quality=high
scrape=youtube
#is_sort=1
#search_sort_by=upload_date
