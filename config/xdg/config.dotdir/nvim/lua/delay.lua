local Delay = {}
Delay.__index = Delay

function Delay.new(self, func)
  local self = {}
  return setmetatable({}, Delay)
end

function Delay.execute(self)
  return self.func()
end

function Delay.isDelay(thing)
  return getmetatable(thing) == Delay
end

return Delay
