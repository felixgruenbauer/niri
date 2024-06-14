// SPDX-License-Identifier: GPL-3.0-only

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use smithay::{
    output::Output,
    reexports::wayland_server::{
        backend::ClientId, Client, DataInit, Dispatch, DisplayHandle, GlobalDispatch, New, Resource,
    },
};
use wayland_backend::protocol::Interface;
use wayland_server::protocol::wl_output::WlOutput;

use crate::niri::{self, State};
use crate::protocols::wayland_protocols::ext::workspace::v0::server::{
    zext_workspace_group_handle_v1::{self, ZextWorkspaceGroupHandleV1},
    zext_workspace_handle_v1::{self, ZextWorkspaceHandleV1, State as WorkspaceStateV0},
    zext_workspace_manager_v1::{self, ZextWorkspaceManagerV1},
};
use crate::protocols::wayland_protocols::ext::workspace::v1::server::{
    ext_workspace_group_handle_v1::{self, ExtWorkspaceGroupHandleV1},
    ext_workspace_handle_v1::{self, ExtWorkspaceHandleV1, State as WorkspaceStateV1},
    ext_workspace_manager_v1::{self, ExtWorkspaceManagerV1},
};

#[derive(Debug)]
pub struct ForeignWorkspaceManagerState {
    dh: DisplayHandle,
    instances: Vec<ManagerHandle>,
    groups: Vec<WorkspaceGroup>,
    //pub workspaces: Vec<Workspace>,
    client_requests: HashMap<ClientId, Vec<Request>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WorkspaceGroup {
    instances: Vec<GroupHandle>,
    pub workspaces: Vec<Workspace>,
    output: Option<Output>,
}

#[derive(Debug, PartialEq, Clone)]
enum ManagerHandle {
    V1(ExtWorkspaceManagerV1),
    V0(ZextWorkspaceManagerV1),
}

impl ManagerHandle {
    fn client(&self) -> Option<Client> {
        match self {
            ManagerHandle::V1(handle) => handle.client(),
            ManagerHandle::V0(handle) => handle.client(),
        }
    }
    fn version(&self) -> u32 {
        match self {
            ManagerHandle::V1(handle) => 1,
            ManagerHandle::V0(handle) => 0,
        }
    }
    fn interface(&self) -> &'static Interface {
        match self {
            ManagerHandle::V1(handle) => handle.id().interface(),
            ManagerHandle::V0(handle) => handle.id().interface(),
        }
    }
    fn workspace_group(&self, group: &GroupHandle) {
        match (self, group) {
            (ManagerHandle::V1(manager_handle), GroupHandle::V1(group_handle)) => {
                manager_handle.workspace_group(group_handle)
            }
            (ManagerHandle::V0(manager_handle), GroupHandle::V0(group_handle)) => {
                manager_handle.workspace_group(group_handle)
            }
            (ManagerHandle::V1(_), GroupHandle::V0(_)) => todo!(),
            (ManagerHandle::V0(_), GroupHandle::V1(_)) => todo!(),
        }
    }
    fn done(&self) {
        match self {
            ManagerHandle::V1(handle) => handle.done(),
            ManagerHandle::V0(handle) => handle.done(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GroupHandle {
    V1(ExtWorkspaceGroupHandleV1),
    V0(ZextWorkspaceGroupHandleV1),
}

impl GroupHandle {
    pub fn id(&self) -> u32 {
        match &self {
            GroupHandle::V1(handle) => handle.id().protocol_id(),
            GroupHandle::V0(handle) => handle.id().protocol_id(),
        }
    }
    pub fn workspace(&self, workspace: &WorkspaceHandle) {
        match (&self, workspace) {
            (GroupHandle::V1(group_handle), WorkspaceHandle::V1(workspace_handle)) => {}
            (GroupHandle::V0(group_handle), WorkspaceHandle::V0(workspace_handle)) => {
                group_handle.workspace(workspace_handle);
            }
            (GroupHandle::V1(_), WorkspaceHandle::V0(_)) => todo!(),
            (GroupHandle::V0(_), WorkspaceHandle::V1(_)) => todo!(),
        }
    }
    pub fn output_enter(&self, output: &WlOutput) {
        match &self {
            GroupHandle::V1(handle) => handle.output_enter(output),
            GroupHandle::V0(handle) => handle.output_enter(output),
        }
    }
    pub fn removed(&self) {
        match &self {
            GroupHandle::V1(handle) => handle.removed(),
            GroupHandle::V0(handle) => handle.remove(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Workspace {
    pub instances: Vec<WorkspaceHandle>,
    name: Option<String>,
    //capabilities: Vec<WorkspaceCapabilities>,
    coordinates: Vec<u8>,
    states: HashSet<WorkspaceState>,
    // in order to make it work with both versions, a workspace always has to have a group,
    // but the group does not always have to be assigned to an output
    //group: Arc<&WorkspaceGroup>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WorkspaceHandle {
    V1(ExtWorkspaceHandleV1),
    V0(ZextWorkspaceHandleV1),
}
impl WorkspaceHandle {
    pub fn id(&self) -> u32 {
        match &self {
            WorkspaceHandle::V1(handle) => handle.id().protocol_id(),
            WorkspaceHandle::V0(handle) => handle.id().protocol_id(),
        }
    }
    pub fn state(&self, state: &HashSet<WorkspaceState>) {
        match &self {
            WorkspaceHandle::V1(handle) => {
                handle.state(ext_workspace_handle_v1::State::from_bits_truncate(state.iter().map(|s| s.value()).sum::<u8>() as u32));
            }
            WorkspaceHandle::V0(handle) => {
                handle.state(state.iter().fold(vec![7u8, 0, 0, 0], |mut acc, s| {
                    match s{
                        WorkspaceState::Active => acc[0] = 0,
                        WorkspaceState::Urgent => acc[1] = 0,
                        WorkspaceState::Hidden => acc[2] = 0,
                    };
                    acc
                }
            ))
            }
        }
    }
    pub fn name(&self, name: String) {
        match &self {
            WorkspaceHandle::V1(handle) => handle.name(name),
            WorkspaceHandle::V0(handle) => handle.name(name),
        }
    }
    pub fn coordinates(&self, coordinates: Vec<u8>) {
        match &self {
            WorkspaceHandle::V1(handle) => handle.coordinates(coordinates),
            WorkspaceHandle::V0(handle) => handle.coordinates(coordinates),
        }
    }
    pub fn removed(&self) {
        match &self {
            WorkspaceHandle::V1(handle) => handle.removed(),
            WorkspaceHandle::V0(handle) => handle.remove(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum WorkspaceState {
    Active,
    Urgent,
    Hidden,
}

impl WorkspaceState {
    fn value(&self) -> u8 {
        match self {
            WorkspaceState::Active => 1,
            WorkspaceState::Urgent => 2,
            WorkspaceState::Hidden => 4,
        }
    }
}

pub trait ForeignWorkspaceHandler {
    fn foreign_workspace_state(&self) -> &ForeignWorkspaceManagerState;
    fn foreign_workspace_state_mut(&mut self) -> &mut ForeignWorkspaceManagerState;
    fn commit_requests(&mut self, dh: &DisplayHandle, requests: Vec<Request>);
}

pub struct ForeignWorkspaceGlobalData {
    filter: Arc<dyn for<'a> Fn(&'a Client) -> bool + Send + Sync>,
}

#[derive(Debug, Clone)]
pub enum Request {
    Activate(WorkspaceHandle),
    Deactivate(WorkspaceHandle),
    Remove(WorkspaceHandle),
    Destroy(WorkspaceHandle),
    Assign {
        workspace: WorkspaceHandle,
        group: GroupHandle,
    },
    Create {
        group: WorkspaceGroup,
        name: Option<String>,
    },
}

impl<D> GlobalDispatch<ExtWorkspaceManagerV1, ForeignWorkspaceGlobalData, D>
    for ForeignWorkspaceManagerState
where
    D: ForeignWorkspaceDispatch,
{
    fn bind(
        state: &mut D,
        dh: &DisplayHandle,
        _client: &Client,
        resource: New<ExtWorkspaceManagerV1>,
        _global_data: &ForeignWorkspaceGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let instance = ManagerHandle::V1(data_init.init(resource, ()));
        send_full_info(state, dh, instance);
    }

    fn can_view(client: Client, global_data: &ForeignWorkspaceGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

fn send_full_info<D: ForeignWorkspaceDispatch>(
    state: &mut D,
    dh: &DisplayHandle,
    manager_instance: ManagerHandle,
) {
    let state = state.foreign_workspace_state_mut();
    for group in state.groups.iter_mut() {
        group.add_instance::<State>(dh, &manager_instance);
        if let Some(client) = manager_instance.client() {
            if let Some(output) = &group.output {
                for wl_output in output.client_outputs(&client) {
                    group.instances[state.instances.len()].output_enter(&wl_output);
                }
            }
        }
        for workspace in group.workspaces.iter_mut() {
            workspace.add_instance::<State>(
                dh,
                &manager_instance,
                &group.instances.last().unwrap().clone(),
            );
            if let Some(name) = &workspace.name {
                workspace
                    .instances
                    .iter()
                    .for_each(|i| i.name(name.clone()));
            }
            if !workspace.coordinates.is_empty() {
                workspace
                    .instances
                    .last()
                    .unwrap_or_else(|| todo!())
                    .coordinates(workspace.coordinates.clone());
            }
            workspace
                .instances
                .last()
                .unwrap_or_else(|| todo!())
                .state(&workspace.states);
            if manager_instance.version() == 1 {
                if let GroupHandle::V1(group_inner) = group.instances.last().unwrap() {
                    if let WorkspaceHandle::V1(workspace_inner) =
                        workspace.instances.last().unwrap()
                    {
                        group_inner.workspace_enter(&workspace_inner);
                    }
                }
            };
        }
    }

    manager_instance.done();
    state.instances.push(manager_instance);
}

impl<D> GlobalDispatch<ZextWorkspaceManagerV1, ForeignWorkspaceGlobalData, D>
    for ForeignWorkspaceManagerState
where
    D: ForeignWorkspaceDispatch,
{
    fn bind(
        state: &mut D,
        dh: &DisplayHandle,
        _client: &Client,
        resource: New<ZextWorkspaceManagerV1>,
        _global_data: &ForeignWorkspaceGlobalData,
        data_init: &mut DataInit<'_, D>,
    ) {
        let instance = ManagerHandle::V0(data_init.init(resource, ()));
        send_full_info(state, dh, instance);
    }

    fn can_view(client: Client, global_data: &ForeignWorkspaceGlobalData) -> bool {
        (global_data.filter)(&client)
    }
}

impl<D> Dispatch<ExtWorkspaceManagerV1, (), D> for ForeignWorkspaceManagerState
where
    D: Dispatch<ExtWorkspaceManagerV1, ()> + ForeignWorkspaceHandler,
{
    fn request(
        state: &mut D,
        client: &Client,
        obj: &ExtWorkspaceManagerV1,
        request: ext_workspace_manager_v1::Request,
        _data: &(),
        dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            ext_workspace_manager_v1::Request::Commit => {
                if let Some(requests) = state
                    .foreign_workspace_state_mut()
                    .client_requests
                    .remove(&client.id())
                {
                    state.commit_requests(dh, requests);
                }
            }
            ext_workspace_manager_v1::Request::Stop => {
                state
                    .foreign_workspace_state_mut()
                    .instances
                    .retain(|i| i != &ManagerHandle::V1(obj.clone()));
            }
        }
    }

    fn destroyed(state: &mut D, _client: ClientId, resource: &ExtWorkspaceManagerV1, _data: &()) {
        state
            .foreign_workspace_state_mut()
            .instances
            .retain(|i| i != &ManagerHandle::V1(resource.clone()));
    }
}

impl<D> Dispatch<ZextWorkspaceManagerV1, (), D> for ForeignWorkspaceManagerState
where
    D: Dispatch<ZextWorkspaceManagerV1, ()> + ForeignWorkspaceHandler,
{
    fn request(
        state: &mut D,
        client: &Client,
        obj: &ZextWorkspaceManagerV1,
        request: zext_workspace_manager_v1::Request,
        _data: &(),
        dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        match request {
            zext_workspace_manager_v1::Request::Commit => {
                if let Some(requests) = state
                    .foreign_workspace_state_mut()
                    .client_requests
                    .remove(&client.id())
                {
                    state.commit_requests(dh, requests);
                }
            }
            zext_workspace_manager_v1::Request::Stop => {
                state
                    .foreign_workspace_state_mut()
                    .instances
                    .retain(|i| i != &ManagerHandle::V0(obj.clone()));
            }
        }
    }

    fn destroyed(state: &mut D, _client: ClientId, resource: &ZextWorkspaceManagerV1, _data: &()) {
        state
            .foreign_workspace_state_mut()
            .instances
            .retain(|i| i != &ManagerHandle::V0(resource.clone()));
    }
}

impl<D> Dispatch<ExtWorkspaceGroupHandleV1, (), D> for ForeignWorkspaceManagerState
where
    D: Dispatch<ExtWorkspaceGroupHandleV1, ()> + ForeignWorkspaceHandler,
{
    fn request(
        state: &mut D,
        client: &Client,
        handle: &ExtWorkspaceGroupHandleV1,
        request: ext_workspace_group_handle_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        if let Some(group) = state
            .foreign_workspace_state_mut()
            .groups
            .iter_mut()
            .find(|g| g.instances.contains(&GroupHandle::V1(handle.clone())))
        {
            let request = match request {
                ext_workspace_group_handle_v1::Request::CreateWorkspace { workspace } => {
                    Request::Create {
                        group: group.clone(),
                        name: Some(workspace),
                    }
                }
                ext_workspace_group_handle_v1::Request::Destroy => {
                    group
                        .instances
                        .retain(|i| i != &GroupHandle::V1(handle.clone()));
                    return;
                }
            };
            state
                .foreign_workspace_state_mut()
                .client_requests
                .entry(client.id())
                .and_modify(|e| e.push(request.clone()))
                .or_insert(vec![request]);
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &ExtWorkspaceGroupHandleV1,
        _data: &(),
    ) {
        for group in &mut state.foreign_workspace_state_mut().groups {
            group
                .instances
                .retain(|i| i != &GroupHandle::V1(resource.clone()))
        }
    }
}

impl<D> Dispatch<ZextWorkspaceGroupHandleV1, (), D> for ForeignWorkspaceManagerState
where
    D: Dispatch<ZextWorkspaceGroupHandleV1, ()> + ForeignWorkspaceHandler,
{
    fn request(
        state: &mut D,
        client: &Client,
        handle: &ZextWorkspaceGroupHandleV1,
        request: zext_workspace_group_handle_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        if let Some(group) = state
            .foreign_workspace_state_mut()
            .groups
            .iter_mut()
            .find(|g| g.instances.contains(&GroupHandle::V0(handle.clone())))
        {
            let request = match request {
                zext_workspace_group_handle_v1::Request::CreateWorkspace { workspace } => {
                    Request::Create {
                        group: group.clone(),
                        name: Some(workspace),
                    }
                }
                zext_workspace_group_handle_v1::Request::Destroy => {
                    group
                        .instances
                        .retain(|i| i != &GroupHandle::V0(handle.clone()));
                    return;
                }
            };
            state
                .foreign_workspace_state_mut()
                .client_requests
                .entry(client.id())
                .and_modify(|e| e.push(request.clone()))
                .or_insert(vec![request]);
        }
    }

    fn destroyed(
        state: &mut D,
        _client: ClientId,
        resource: &ZextWorkspaceGroupHandleV1,
        _data: &(),
    ) {
        for group in &mut state.foreign_workspace_state_mut().groups {
            group
                .instances
                .retain(|i| i != &GroupHandle::V0(resource.clone()))
        }
    }
}

impl<D> Dispatch<ExtWorkspaceHandleV1, (), D> for ForeignWorkspaceManagerState
where
    D: Dispatch<ExtWorkspaceHandleV1, ()> + ForeignWorkspaceHandler,
{
    fn request(
        state: &mut D,
        client: &Client,
        handle: &ExtWorkspaceHandleV1,
        request: ext_workspace_handle_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let handle = WorkspaceHandle::V1(handle.clone());
        {
            let req = match request {
                ext_workspace_handle_v1::Request::Activate => Request::Activate(handle),
                ext_workspace_handle_v1::Request::Deactivate => Request::Deactivate(handle),
                ext_workspace_handle_v1::Request::Remove => Request::Remove(handle),
                ext_workspace_handle_v1::Request::Destroy => Request::Destroy(handle),
                ext_workspace_handle_v1::Request::Assign { workspace_group } => Request::Assign {
                    workspace: handle.clone(),
                    group: GroupHandle::V1(workspace_group),
                },
            };
            state
                .foreign_workspace_state_mut()
                .client_requests
                .entry(client.id())
                .and_modify(|e| e.push(req.clone()))
                .or_insert(vec![req]);
        }
    }

    fn destroyed(state: &mut D, _client: ClientId, resource: &ExtWorkspaceHandleV1, _data: &()) {
        state
            .foreign_workspace_state_mut()
            .groups
            .iter_mut()
            .find_map(|group| {
                group.workspaces.iter_mut().find(|ws| {
                    ws.instances
                        .contains(&WorkspaceHandle::V1(resource.clone()))
                })
            })
            .map(|ws| {
                ws.instances
                    .retain(|i| i != &WorkspaceHandle::V1(resource.clone()))
            });
    }
}

impl<D> Dispatch<ZextWorkspaceHandleV1, (), D> for ForeignWorkspaceManagerState
where
    D: Dispatch<ZextWorkspaceHandleV1, ()> + ForeignWorkspaceHandler,
{
    fn request(
        state: &mut D,
        client: &Client,
        handle: &ZextWorkspaceHandleV1,
        request: zext_workspace_handle_v1::Request,
        _data: &(),
        _dh: &DisplayHandle,
        _data_init: &mut DataInit<'_, D>,
    ) {
        let handle = WorkspaceHandle::V0(handle.clone());
        let req = match request {
            zext_workspace_handle_v1::Request::Activate => Request::Activate(handle),
            zext_workspace_handle_v1::Request::Deactivate => Request::Deactivate(handle),
            zext_workspace_handle_v1::Request::Remove => Request::Remove(handle),
            zext_workspace_handle_v1::Request::Destroy => Request::Destroy(handle),
        };
        state
            .foreign_workspace_state_mut()
            .client_requests
            .entry(client.id())
            .and_modify(|e| e.push(req.clone()))
            .or_insert(vec![req]);
    }

    fn destroyed(state: &mut D, _client: ClientId, resource: &ZextWorkspaceHandleV1, _data: &()) {
        state
            .foreign_workspace_state_mut()
            .groups
            .iter_mut()
            .find_map(|group| {
                group.workspaces.iter_mut().find(|ws| {
                    ws.instances
                        .contains(&WorkspaceHandle::V0(resource.clone()))
                })
            })
            .map(|ws| {
                ws.instances
                    .retain(|i| i != &WorkspaceHandle::V0(resource.clone()))
            });
    }
}

pub trait ForeignWorkspaceDispatch:
    GlobalDispatch<ZextWorkspaceManagerV1, ForeignWorkspaceGlobalData>
    + Dispatch<ZextWorkspaceHandleV1, ()>
    + Dispatch<ZextWorkspaceGroupHandleV1, ()>
    + Dispatch<ZextWorkspaceManagerV1, ()>
    + GlobalDispatch<ExtWorkspaceManagerV1, ForeignWorkspaceGlobalData>
    + Dispatch<ExtWorkspaceHandleV1, ()>
    + Dispatch<ExtWorkspaceGroupHandleV1, ()>
    + Dispatch<ExtWorkspaceManagerV1, ()>
    + ForeignWorkspaceHandler
    + 'static
{
}

impl<T> ForeignWorkspaceDispatch for T where
    T: GlobalDispatch<ZextWorkspaceManagerV1, ForeignWorkspaceGlobalData>
        + Dispatch<ZextWorkspaceHandleV1, ()>
        + Dispatch<ZextWorkspaceGroupHandleV1, ()>
        + Dispatch<ZextWorkspaceManagerV1, ()>
        + GlobalDispatch<ExtWorkspaceManagerV1, ForeignWorkspaceGlobalData>
        + Dispatch<ExtWorkspaceHandleV1, ()>
        + Dispatch<ExtWorkspaceGroupHandleV1, ()>
        + Dispatch<ExtWorkspaceManagerV1, ()>
        + ForeignWorkspaceHandler
        + 'static
{
}

const VERSION: u32 = 1;
impl ForeignWorkspaceManagerState {
    pub fn new<D, F>(display: &DisplayHandle, client_filter: F) -> Self
    where
        D: ForeignWorkspaceDispatch,
        F: for<'a> Fn(&'a Client) -> bool + Send + Sync + 'static,
    {
        let filter = Arc::new(client_filter);
        let global_data = ForeignWorkspaceGlobalData {
            filter: filter.clone(),
        };
        display.create_global::<D, ZextWorkspaceManagerV1, _>(VERSION, global_data);
        let global_data = ForeignWorkspaceGlobalData { filter };
        display.create_global::<D, ExtWorkspaceManagerV1, _>(VERSION, global_data);
        Self {
            dh: display.clone(),
            instances: Vec::new(),
            groups: Vec::new(),
            client_requests: HashMap::new(),
        }
    }

    pub fn group_workspace_from_handle(
        &self,
        workspace_handle: &WorkspaceHandle,
    ) -> (&WorkspaceGroup, &Workspace) {
        self.groups
            .iter()
            .find_map(|group| {
                if let Some(ws) = group
                    .workspaces
                    .iter()
                    .find(|ws| ws.instances.contains(workspace_handle))
                {
                    Some((group, ws))
                } else {
                    None
                }
            })
            .unwrap()
    }
}
impl Workspace {
    fn add_instance<D>(&mut self, dh: &DisplayHandle, manager: &ManagerHandle, group: &GroupHandle)
    where
        D: Dispatch<ZextWorkspaceHandleV1, ()>,
        D: Dispatch<ExtWorkspaceHandleV1, ()>,
        D: 'static,
    {
        if let Some(client) = manager.client() {
            if let Some(handle) = match manager {
                ManagerHandle::V1(manager_handle) => {
                    if let Some(workspace_handle) = client
                        .create_resource::<ExtWorkspaceHandleV1, (), D>(&dh, 1u32, ())
                        .ok()
                    {
                        manager_handle.workspace(&workspace_handle);
                        if let GroupHandle::V1(group_handle) = group {
                            group_handle.workspace_enter(&workspace_handle);
                        }
                        Some(WorkspaceHandle::V1(workspace_handle))
                    } else {
                        None
                    }
                }
                ManagerHandle::V0(_) => {
                    if let Some(workspace_handle) = client
                        .create_resource::<ZextWorkspaceHandleV1, (), D>(&dh, 1u32, ())
                        .map_or(None, |h| Some(h))
                    {
                        if let GroupHandle::V0(group_handle) = group {
                            group_handle.workspace(&workspace_handle);
                        }
                        Some(WorkspaceHandle::V0(workspace_handle))
                    } else {
                        None
                    }
                }
            } {
                self.instances.push(handle);
            }
        }
    }
}

impl WorkspaceGroup {
    fn add_instance<D>(&mut self, dh: &DisplayHandle, manager: &ManagerHandle)
    where
        D: Dispatch<ZextWorkspaceGroupHandleV1, ()>,
        D: Dispatch<ExtWorkspaceGroupHandleV1, ()>,
        D: 'static,
    {
        if let Some(client) = manager.client() {
            if let Some(handle) = match manager.version() {
                1 => client
                    .create_resource::<ExtWorkspaceGroupHandleV1, (), D>(&dh, 1u32, ())
                    .map_or(None, |h| Some(GroupHandle::V1(h))),
                0 => client
                    .create_resource::<ZextWorkspaceGroupHandleV1, (), D>(&dh, 1u32, ())
                    .map_or(None, |h| Some(GroupHandle::V0(h))),
                _ => None,
            } {
                manager.workspace_group(&handle);
                self.instances.push(handle);
            }
        }
    }
}

pub fn refresh<D>(state: &mut State)
where
    D: Dispatch<ZextWorkspaceGroupHandleV1, ()> + Dispatch<ZextWorkspaceHandleV1, ()> + 'static,
{
    let protocol_state = &mut state.niri.foreign_workspace_state;
    let mut changed = false;
    
    let ipc_workspaces = &state.niri.layout.ipc_workspaces();
    
    debug!("ipc workspaces info: {:?}", ipc_workspaces);

    // remove groups that are assigned to an output, which is no longer valid/existing
    // first for all workspaces in group send instance.remove then destroy workspace struct
    // then for the group send instance.remove and detroy group object
    protocol_state.groups.retain(|group| {
        if ipc_workspaces
            .iter()
            .find(|ws| ws.output == group.output.as_ref().map(|o| o.name()))
            .is_none()
        {
            group
                .workspaces
                .iter()
                .for_each(|w| w.instances.iter().for_each(|i| i.removed()));
            group.instances.iter().for_each(|i| i.removed());
            changed = true;
            false
        } else {
            true
        }
    });

    // remove workspaces from groups to reach the workspaces target count from niri
    for group in protocol_state.groups.iter_mut() {
        let target_count = ipc_workspaces
            .iter()
            .filter(|ws| ws.output == group.output.as_ref().map(|o| o.name()))
            .count();
        for workspace in group.workspaces.drain(target_count.min(group.workspaces.len())..) {
            workspace.instances.iter().for_each(|i| i.removed());
            changed = true;
        }
    }

    for niri_workspace in ipc_workspaces.iter() {
        let group = if let Some(group) = protocol_state
            .groups
            .iter_mut()
            .find(|g| g.output.as_ref().map(|o| o.name()) == niri_workspace.output)
        {
            group
        } else {
            let output = niri_workspace.output.as_ref().and_then(|n| {
                state
                    .niri
                    .layout
                    .outputs()
                    .into_iter()
                    .find(|o| o.name() == *n)
            });
            protocol_state.groups.push(WorkspaceGroup {
                instances: Vec::new(),
                output: output.cloned(),
                workspaces: Vec::new(),
            });
            let group = protocol_state.groups.last_mut().unwrap();
            for manager in protocol_state.instances.iter() {
                group.add_instance::<State>(&protocol_state.dh, manager);
            }
            if let Some(output) = output {
                for manager in protocol_state.instances.iter() {
                    if let Some(client) = manager.client() {
                        for wl_output in output.client_outputs(&client).iter() {
                            group
                                .instances
                                .iter()
                                .for_each(|i| i.output_enter(wl_output));
                        }
                    }
                }
            }
            changed = true;
            group
        };
        let workspace = if let Some(workspace) =
            group.workspaces.get_mut((niri_workspace.idx - 1) as usize)
        {
            workspace
        } else {
            group.workspaces.push(Workspace {
                instances: Vec::new(),
                name: None,
                coordinates: Vec::new(),
                states: HashSet::new(),
            });
            let workspace = group.workspaces.last_mut().unwrap();
            for (idx, manager) in protocol_state.instances.iter().enumerate() {
                workspace.add_instance::<State>(&protocol_state.dh, manager, &group.instances[idx])
            }
            changed = true;
            workspace
        };
        if workspace.name != niri_workspace.name {
            workspace.name = niri_workspace.name.clone();
            workspace
                .instances
                .iter()
                .for_each(|i| i.name(workspace.name.clone().unwrap_or("".to_string())));
            changed = true;
        }
        if let Some(coordinate) = workspace.coordinates.get_mut(0) {
            if coordinate != &(niri_workspace.idx - 1) {
                *coordinate = niri_workspace.idx - 1;
                workspace
                    .instances
                    .iter()
                    .for_each(|i| i.coordinates(workspace.coordinates.clone()));
                changed = true;
            }
        } else {
            workspace.coordinates.push(niri_workspace.idx - 1);
            workspace
                .instances
                .iter()
                .for_each(|i| i.coordinates(workspace.coordinates.clone()));
            changed = true;
        }
        if niri_workspace.is_active {
            if !workspace.states.contains(&WorkspaceState::Active) {
                workspace.states.insert(WorkspaceState::Active);
                workspace
                    .instances
                    .iter()
                    .for_each(|i| i.state(&workspace.states));
                changed = true;
            }
        } else {
            if workspace.states.contains(&WorkspaceState::Active) {
                workspace.states.remove(&WorkspaceState::Active);
                changed = true;
                // state change event
                workspace.instances.iter().for_each(|i| i.state(&workspace.states));
            }
        }
    }

    if changed {
        for manager in protocol_state.instances.iter() {
            manager.done()
        }
    }
}

macro_rules! delegate_workspace {
    ($(@<$( $lt:tt $( : $clt:tt $(+ $dlt:tt )* )? ),+>)? $ty: ty) => {
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            crate::protocols::wayland_protocols::ext::workspace::v1::server::ext_workspace_manager_v1::ExtWorkspaceManagerV1: $crate::protocols::workspace::ForeignWorkspaceGlobalData
        ] => $crate::protocols::workspace::ForeignWorkspaceManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            crate::protocols::wayland_protocols::ext::workspace::v1::server::ext_workspace_manager_v1::ExtWorkspaceManagerV1: ()
        ] => $crate::protocols::workspace::ForeignWorkspaceManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            crate::protocols::wayland_protocols::ext::workspace::v1::server::ext_workspace_group_handle_v1::ExtWorkspaceGroupHandleV1: ()
        ] => $crate::protocols::workspace::ForeignWorkspaceManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            crate::protocols::wayland_protocols::ext::workspace::v1::server::ext_workspace_handle_v1::ExtWorkspaceHandleV1: ()
        ] => $crate::protocols::workspace::ForeignWorkspaceManagerState);
        smithay::reexports::wayland_server::delegate_global_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            crate::protocols::wayland_protocols::ext::workspace::v0::server::zext_workspace_manager_v1::ZextWorkspaceManagerV1: $crate::protocols::workspace::ForeignWorkspaceGlobalData
        ] => $crate::protocols::workspace::ForeignWorkspaceManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            crate::protocols::wayland_protocols::ext::workspace::v0::server::zext_workspace_manager_v1::ZextWorkspaceManagerV1: ()
        ] => $crate::protocols::workspace::ForeignWorkspaceManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            crate::protocols::wayland_protocols::ext::workspace::v0::server::zext_workspace_group_handle_v1::ZextWorkspaceGroupHandleV1: ()
        ] => $crate::protocols::workspace::ForeignWorkspaceManagerState);
        smithay::reexports::wayland_server::delegate_dispatch!($(@< $( $lt $( : $clt $(+ $dlt )* )? ),+ >)? $ty: [
            crate::protocols::wayland_protocols::ext::workspace::v0::server::zext_workspace_handle_v1::ZextWorkspaceHandleV1: ()
        ] => $crate::protocols::workspace::ForeignWorkspaceManagerState);
    };
}
pub(crate) use delegate_workspace;
