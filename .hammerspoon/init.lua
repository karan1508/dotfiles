-- Modifiers 
local modifiers = {"cmd", "ctrl"}

-- load spoons
aclock = hs.loadSpoon("AClock")
hs.hotkey.bind(modifiers, 'l', function() aclock:toggleShow() end)


--- Defeating paste blocking  
hs.hotkey.bind(modifiers, "V", function() hs.eventtap.keyStrokes(hs.pasteboard.getContents()) end)

-- Code for finding the mouse pointer on the screen
mouseCircle = nil
mouseCircleTimer = nil

function mouseHighlight()
    -- Delete an existing highlight if it exists
    if mouseCircle then
        mouseCircle:delete()
        if mouseCircleTimer then
            mouseCircleTimer:stop()
        end
    end
    -- Get the current co-ordinates of the mouse pointer
    mousepoint = hs.mouse.getAbsolutePosition()
    -- Prepare a big red circle around the mouse pointer
    mouseCircle = hs.drawing.circle(hs.geometry.rect(mousepoint.x-40, mousepoint.y-40, 80, 80))
    mouseCircle:setStrokeColor({["red"]=1,["blue"]=0,["green"]=0,["alpha"]=1})
    mouseCircle:setFill(false)
    mouseCircle:setStrokeWidth(5)
    mouseCircle:show()

    -- Set a timer to delete the circle after 3 seconds
    mouseCircleTimer = hs.timer.doAfter(3, function() mouseCircle:delete() end)
end
hs.hotkey.bind(modifiers, "M", mouseHighlight)

-------------------------------------
-- Application focusing shortcuts
-------------------------------------

-- Like hs.application.launchOrFocus, except that it works for apps created using Epichrome.
-- I'm not sure why this implementation has different behavior than hs.application.launchOrFocus.
-- Reference: https://github.com/Hammerspoon/hammerspoon/issues/304
function myLaunchOrFocus(appName)
    local app = hs.appfinder.appFromName(appName)
    if app == nil then
       hs.application.launchOrFocus(appName)
    else
       windows = app:allWindows()
       if windows[1] then
          windows[1]:focus()
       end
    end
 end
 hs.hotkey.bind(modifiers, ',', function() myLaunchOrFocus('Slack') end)
 hs.hotkey.bind(modifiers, 'c', function() myLaunchOrFocus('Visual Studio Code') end)
 hs.hotkey.bind(modifiers, 't', function() myLaunchOrFocus('iTerm') end)
 hs.hotkey.bind(modifiers, 'b', function() myLaunchOrFocus('Google Chrome') end)


 -- Layouts
-- currentLayout = null
-- layouts = {
--  small={
--     {"Google Chrome", nil, screen, hs.layout.left50, nil, nil},
--     {"Slack", nil, screen, hs.layout.right50, nil, nil},
--   },
-- }

-- function applyLayout(layout)
--     local screen = hs.screen.mainScreen()
--     local layoutsize = layout.small

--     currentlayout = layout
--     hs.layout.apply(layoutsize)

--  end

--  hs.hotkey.bind(modifiers, 'i', function() applyLayout(layouts) end)


 -- applications
 appchooser = hs.chooser.new(function(selection)
   if not selection then return end
   myLaunchOrFocus(selection.text)
 end)
 i = 0
 appchooser:choices(hs.fnutils.imap(hs.application.runningApplications(), function(app)
   i = i + 1
   return {
      index=i,
      text=app:title(),
      subtext=app:title()
   }
 end))
 appchooser:width(20)
appchooser:subTextColor({red=0, green=0, blue=0, alpha=0.4})
hs.hotkey.bind(modifiers, 'i', function() appchoser:show() end)


-- config relaod
hs.loadSpoon("ReloadConfiguration")
spoon.ReloadConfiguration:start()
hs.alert.show("Config reloaded")

-- iterm transparency
function toggleItermTransparency()
 script =   [[tell application "iTerm"
    if the transparency of the current session of the current window > 0 then
        repeat with aWindow in windows
            tell aWindow
                repeat with aTab in tabs of aWindow
                    repeat with aSession in sessions of aTab
                        tell aSession
                            set transparency to 0
                        end tell
                    end repeat
                end repeat
            end tell
        end repeat
    else
        repeat with aWindow in windows
            tell aWindow
                repeat with aTab in tabs of aWindow
                    repeat with aSession in sessions of aTab
                        tell aSession
                            set transparency to 0.3
                        end tell
                    end repeat
                end repeat
            end tell
        end repeat
    end if
 end tell ]]

 ok, result = hs.applescript(script)
end        
hs.hotkey.bind(modifiers, 'k', function() toggleItermTransparency() end)

-- watchers 
function changed(files)
   for i = 1, #files do
      print(files[i])
   end
end
function folderWatcher(folder_path)
   hs.pathwatcher.new(folder_path, changed):start()
end

