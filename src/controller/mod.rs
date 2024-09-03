pub(crate) mod item_impl;
pub(crate) mod item_struct;

const ALL_CHANNEL_CAPACITY: usize = 8;
const SIGNAL_CHANNEL_CAPACITY: usize = 8;
const BROADCAST_MAX_PUBLISHERS: usize = 1;
const BROADCAST_MAX_SUBSCRIBERS: usize = 16;
