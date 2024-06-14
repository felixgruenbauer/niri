//! Miscellaneous protocols

#![cfg_attr(rustfmt, rustfmt_skip)]

#[allow(missing_docs)]
pub mod workspace {
    pub mod v1 {
        pub mod server {
            use smithay::reexports::wayland_server;
            // import objects from the core protocol if needed
            use wayland_server::protocol::*;
            
            // This module hosts a low-level representation of the protocol objects
            // you will not need to interact with it yourself, but the code generated
            // by the generate_client_code! macro will use it
            pub mod __interfaces {
                // import the interfaces from the core protocol if needed
                use wayland_server::protocol::__interfaces::*;
                wayland_scanner::generate_interfaces!("src/protocols/wayland_protocols/ext-workspace-v1.xml");
            }
            use self::__interfaces::*;
            
            // This macro generates the actual types that represent the wayland objects of
            // your custom protocol
            wayland_scanner::generate_server_code!("src/protocols/wayland_protocols/ext-workspace-v1.xml");
        }
    }
    pub mod v0 {
        pub mod server {
            use smithay::reexports::wayland_server;
            // import objects from the core protocol if needed
            use wayland_server::protocol::*;
            
            // This module hosts a low-level representation of the protocol objects
            // you will not need to interact with it yourself, but the code generated
            // by the generate_client_code! macro will use it
            pub mod __interfaces {
                // import the interfaces from the core protocol if needed
                use wayland_server::protocol::__interfaces::*;
                wayland_scanner::generate_interfaces!("src/protocols/wayland_protocols/zext-workspace-unstable-v1.xml");
            }
            use self::__interfaces::*;
            
            // This macro generates the actual types that represent the wayland objects of
            // your custom protocol
            wayland_scanner::generate_server_code!("src/protocols/wayland_protocols/zext-workspace-unstable-v1.xml");
        }
    }
}
