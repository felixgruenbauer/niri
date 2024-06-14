// SPDX-License-Identifier: GPL-3.0-only

use crate::protocols::workspace::{
    delegate_workspace, Request, ForeignWorkspaceHandler, ForeignWorkspaceManagerState,
};
use smithay::reexports::wayland_server::DisplayHandle;
use crate::niri::State;


impl ForeignWorkspaceHandler for State {
fn foreign_workspace_state(&self) -> &ForeignWorkspaceManagerState {
    &self.niri.foreign_workspace_state
}
fn foreign_workspace_state_mut(&mut self) -> &mut ForeignWorkspaceManagerState {
    &mut self.niri.foreign_workspace_state
}

fn commit_requests(&mut self, _dh: &DisplayHandle, requests: Vec<Request>) {
    for request in requests.into_iter() {
        match request {
            Request::Activate(handle) => {
                let (group, workspace) = self.foreign_workspace_state().group_workspace_from_handle(&handle);
                if let Some(idx) = group.workspaces.iter().position(|ws| workspace == ws) {
                    self.niri.layout.switch_workspace(idx);
                }
            }
            _ => {}
        }
    }
}
}

delegate_workspace!(State);
