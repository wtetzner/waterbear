//! Declarative configuration for packaging WebAssembly as a VMU program.

use std::{
    error::Error,
    fmt, fs,
    path::{Path, PathBuf},
};

use serde::Deserialize;

const DEFAULT_VMU_DESCRIPTION: &str = "Waterbear WASM";
const DEFAULT_DREAMCAST_DESCRIPTION: &str = "Waterbear WebAssembly Program";
const DEFAULT_APPLICATION_ID: &str = "waterbear-wasm";

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(default, deny_unknown_fields)]
pub struct BuildConfig {
    pub wasm: WasmConfig,
    pub package: PackageConfig,
    pub interrupts: InterruptConfig,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            wasm: WasmConfig::default(),
            package: PackageConfig::default(),
            interrupts: InterruptConfig::default(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(default, deny_unknown_fields)]
pub struct WasmConfig {
    pub entry: String,
}

impl Default for WasmConfig {
    fn default() -> Self {
        Self {
            entry: "main".to_owned(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(default, deny_unknown_fields)]
pub struct PackageConfig {
    pub vmu_description: String,
    pub dreamcast_description: String,
    pub application_id: String,
    pub icon: IconConfig,
}

impl Default for PackageConfig {
    fn default() -> Self {
        Self {
            vmu_description: DEFAULT_VMU_DESCRIPTION.to_owned(),
            dreamcast_description: DEFAULT_DREAMCAST_DESCRIPTION.to_owned(),
            application_id: DEFAULT_APPLICATION_ID.to_owned(),
            icon: IconConfig::default(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(default, deny_unknown_fields)]
pub struct IconConfig {
    pub path: Option<PathBuf>,
    pub speed: u16,
    pub eyecatch: Option<PathBuf>,
}

impl Default for IconConfig {
    fn default() -> Self {
        Self {
            path: None,
            speed: 10,
            eyecatch: None,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Eq)]
#[serde(default, deny_unknown_fields)]
pub struct InterruptConfig {
    pub external_0: Option<String>,
    pub external_1: Option<String>,
    pub external_2_or_timer_0_low: Option<String>,
    pub external_3_or_base_timer: Option<String>,
    pub timer_0_high: Option<String>,
    pub timer_1: Option<String>,
    pub serial_0: Option<String>,
    pub serial_1: Option<String>,
    pub dreamcast_interface: Option<String>,
    pub port_3: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InterruptSlot {
    External0,
    External1,
    External2OrTimer0Low,
    External3OrBaseTimer,
    Timer0High,
    Timer1,
    Serial0,
    Serial1,
    DreamcastInterface,
    Port3,
}

impl InterruptSlot {
    pub const ALL: [Self; 10] = [
        Self::External0,
        Self::External1,
        Self::External2OrTimer0Low,
        Self::External3OrBaseTimer,
        Self::Timer0High,
        Self::Timer1,
        Self::Serial0,
        Self::Serial1,
        Self::DreamcastInterface,
        Self::Port3,
    ];

    pub fn name(self) -> &'static str {
        match self {
            Self::External0 => "external_0",
            Self::External1 => "external_1",
            Self::External2OrTimer0Low => "external_2_or_timer_0_low",
            Self::External3OrBaseTimer => "external_3_or_base_timer",
            Self::Timer0High => "timer_0_high",
            Self::Timer1 => "timer_1",
            Self::Serial0 => "serial_0",
            Self::Serial1 => "serial_1",
            Self::DreamcastInterface => "dreamcast_interface",
            Self::Port3 => "port_3",
        }
    }
}

impl InterruptConfig {
    pub fn get(&self, slot: InterruptSlot) -> Option<&str> {
        match slot {
            InterruptSlot::External0 => self.external_0.as_deref(),
            InterruptSlot::External1 => self.external_1.as_deref(),
            InterruptSlot::External2OrTimer0Low => self.external_2_or_timer_0_low.as_deref(),
            InterruptSlot::External3OrBaseTimer => self.external_3_or_base_timer.as_deref(),
            InterruptSlot::Timer0High => self.timer_0_high.as_deref(),
            InterruptSlot::Timer1 => self.timer_1.as_deref(),
            InterruptSlot::Serial0 => self.serial_0.as_deref(),
            InterruptSlot::Serial1 => self.serial_1.as_deref(),
            InterruptSlot::DreamcastInterface => self.dreamcast_interface.as_deref(),
            InterruptSlot::Port3 => self.port_3.as_deref(),
        }
    }
}

#[derive(Debug)]
pub enum ConfigError {
    Read {
        path: PathBuf,
        source: std::io::Error,
    },
    Parse {
        path: PathBuf,
        source: toml::de::Error,
    },
    Invalid(String),
}

impl fmt::Display for ConfigError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Read { path, source } => {
                write!(
                    formatter,
                    "could not read config {}: {source}",
                    path.display()
                )
            }
            Self::Parse { path, source } => {
                write!(
                    formatter,
                    "could not parse config {}: {source}",
                    path.display()
                )
            }
            Self::Invalid(message) => formatter.write_str(message),
        }
    }
}

impl Error for ConfigError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Read { source, .. } => Some(source),
            Self::Parse { source, .. } => Some(source),
            Self::Invalid(_) => None,
        }
    }
}

impl BuildConfig {
    pub fn load(path: Option<&Path>) -> Result<Self, ConfigError> {
        let Some(path) = path else {
            let config = Self::default();
            config.validate()?;
            return Ok(config);
        };

        let absolute_path = if path.is_absolute() {
            path.to_owned()
        } else {
            std::env::current_dir()
                .map_err(|source| ConfigError::Read {
                    path: path.to_owned(),
                    source,
                })?
                .join(path)
        };
        let source = fs::read_to_string(&absolute_path).map_err(|source| ConfigError::Read {
            path: absolute_path.clone(),
            source,
        })?;
        let mut config: Self = toml::from_str(&source).map_err(|source| ConfigError::Parse {
            path: absolute_path.clone(),
            source,
        })?;
        let directory = absolute_path.parent().unwrap_or_else(|| Path::new("."));
        config.resolve_paths(directory);
        config.validate()?;
        Ok(config)
    }

    pub fn entry<'a>(&'a self, override_name: Option<&'a str>) -> &'a str {
        override_name.unwrap_or(&self.wasm.entry)
    }

    fn resolve_paths(&mut self, directory: &Path) {
        for path in [
            self.package.icon.path.as_mut(),
            self.package.icon.eyecatch.as_mut(),
        ]
        .into_iter()
        .flatten()
        {
            if path.is_relative() {
                *path = directory.join(&*path);
            }
        }
    }

    fn validate(&self) -> Result<(), ConfigError> {
        validate_field("wasm.entry", &self.wasm.entry, None)?;
        validate_field(
            "package.vmu_description",
            &self.package.vmu_description,
            Some(16),
        )?;
        validate_field(
            "package.dreamcast_description",
            &self.package.dreamcast_description,
            Some(32),
        )?;
        validate_field(
            "package.application_id",
            &self.package.application_id,
            Some(16),
        )?;
        if self.package.icon.path.is_none() && self.package.icon.eyecatch.is_some() {
            return Err(ConfigError::Invalid(
                "package.icon.eyecatch requires package.icon.path".to_owned(),
            ));
        }
        for slot in InterruptSlot::ALL {
            if let Some(function) = self.interrupts.get(slot) {
                validate_field(&format!("interrupts.{}", slot.name()), function, None)?;
            }
        }
        Ok(())
    }
}

fn validate_field(
    name: &str,
    value: &str,
    maximum_bytes: Option<usize>,
) -> Result<(), ConfigError> {
    if value.is_empty() {
        return Err(ConfigError::Invalid(format!("{name} must not be empty")));
    }
    if let Some(maximum_bytes) = maximum_bytes {
        if value.len() > maximum_bytes {
            return Err(ConfigError::Invalid(format!(
                "{name} is {} bytes, but its VMU field holds at most {maximum_bytes}",
                value.len()
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::fs;

    use assert_fs::TempDir;

    use super::BuildConfig;

    #[test]
    fn omitted_sections_use_defaults() {
        let config: BuildConfig = toml::from_str("").unwrap();
        assert_eq!(config.wasm.entry, "main");
        assert_eq!(config.package.vmu_description, "Waterbear WASM");
        assert_eq!(config.package.icon.speed, 10);
        assert!(config.package.icon.path.is_none());
    }

    #[test]
    fn reads_package_and_interrupt_settings() {
        let config: BuildConfig = toml::from_str(
            r#"
                [wasm]
                entry = "start"

                [package]
                vmu_description = "Test"
                dreamcast_description = "Test Program"
                application_id = "test"

                [package.icon]
                path = "icon.png"
                speed = 12

                [interrupts]
                timer_0_high = "tick"
            "#,
        )
        .unwrap();
        assert_eq!(config.wasm.entry, "start");
        assert_eq!(config.package.icon.speed, 12);
        assert_eq!(config.interrupts.timer_0_high.as_deref(), Some("tick"));
    }

    #[test]
    fn rejects_unknown_fields() {
        let error = toml::from_str::<BuildConfig>("mystery = true").unwrap_err();
        assert!(error.to_string().contains("unknown field"));
    }

    #[test]
    fn resolves_asset_paths_relative_to_the_config_file() {
        let directory = TempDir::new().unwrap();
        let path = directory.path().join("waterbear.toml");
        fs::write(
            &path,
            r#"
                [package.icon]
                path = "images/icon.gif"
                eyecatch = "images/eyecatch.png"
            "#,
        )
        .unwrap();

        let config = BuildConfig::load(Some(&path)).unwrap();

        assert_eq!(
            config.package.icon.path.as_deref(),
            Some(directory.path().join("images/icon.gif").as_path())
        );
        assert_eq!(
            config.package.icon.eyecatch.as_deref(),
            Some(directory.path().join("images/eyecatch.png").as_path())
        );
    }
}
