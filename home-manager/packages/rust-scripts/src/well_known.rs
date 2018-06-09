
pub Trait {
    fn definition() -> Task
    fn is_task(&Task) -> bool
    fn action_necessary(&TaskCache, State) -> Result<bool>
    fn process(&mut TaskCache) -> Result<()>
    fn refresh() -> refresh::Timer
}
