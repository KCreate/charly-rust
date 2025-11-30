// MIT License
//
// Copyright (c) 2025 Leonard Schütz
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use ariadne::{
    Color, ColorGenerator, Config, IndexType, Label, LabelAttach, Report, ReportKind,
    Source,
};
use std::ops::Range;
use std::path::PathBuf;

use crate::charly::utils::window_buffer::TextSpan;

pub type FileId = usize;

#[derive(Debug, Clone, Default)]
pub struct DiagnosticLocation {
    pub file_id: FileId,
    pub span: TextSpan,
}

impl DiagnosticLocation {
    pub fn merge(&self, other: &DiagnosticLocation) -> DiagnosticLocation {
        assert_eq!(self.file_id, other.file_id);
        DiagnosticLocation {
            file_id: self.file_id,
            span: TextSpan {
                start: self.span.start.min(other.span.start),
                end: self.span.end.max(other.span.end),
            },
        }
    }
}

impl DiagnosticLocation {
    pub fn offset_range(&self) -> Range<usize> {
        let start_offset = self.span.start.byte_offset;
        let end_offset = self.span.end.byte_offset;
        start_offset..end_offset
    }
}

pub struct DiagnosticSourceFile {
    pub path: PathBuf,
    pub source: String,
}

pub struct DiagnosticController {
    file_sources: Vec<Box<DiagnosticSourceFile>>,
    pub contexts: Vec<Box<DiagnosticContext>>,
}

pub struct DiagnosticContext {
    pub file_id: FileId,
    pub messages: Vec<DiagnosticMessage>,
}

#[derive(Debug)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Advice,
}

impl From<&DiagnosticSeverity> for ReportKind<'_> {
    fn from(severity: &DiagnosticSeverity) -> Self {
        match severity {
            DiagnosticSeverity::Error => ReportKind::Error,
            DiagnosticSeverity::Warning => ReportKind::Warning,
            DiagnosticSeverity::Advice => ReportKind::Advice,
        }
    }
}

impl From<&DiagnosticSeverity> for Color {
    fn from(severity: &DiagnosticSeverity) -> Self {
        match severity {
            DiagnosticSeverity::Error => Color::Red,
            DiagnosticSeverity::Warning => Color::Yellow,
            DiagnosticSeverity::Advice => Color::Blue,
        }
    }
}

pub struct DiagnosticMessage {
    pub location: DiagnosticLocation,
    pub severity: DiagnosticSeverity,
    pub title: String,
    pub labels: Vec<DiagnosticLabel>,
    pub notes: Vec<String>,
}

pub struct DiagnosticLabel {
    pub location: DiagnosticLocation,
    pub label: Option<String>,
}

impl DiagnosticController {
    pub fn new() -> Self {
        Self {
            file_sources: Vec::new(),
            contexts: Vec::new(),
        }
    }

    pub fn register_file(&mut self, path: &PathBuf, source: &str) -> FileId {
        let id = self.file_sources.len();
        let source_file = Box::new(DiagnosticSourceFile {
            path: path.clone(),
            source: source.to_string(),
        });
        self.file_sources.push(source_file);
        id
    }

    pub fn get_file(&self, file_id: FileId) -> Option<&DiagnosticSourceFile> {
        let source_file = self.file_sources.get(file_id);
        match source_file {
            Some(source_file) => Some(source_file),
            None => None,
        }
    }

    pub fn get_or_create_context(&mut self, file_id: FileId) -> &mut DiagnosticContext {
        let index = self
            .contexts
            .iter()
            .position(|context| context.file_id == file_id);
        match index {
            Some(index) => self.contexts.get_mut(index).unwrap(),
            None => {
                self.contexts.push(Box::new(DiagnosticContext {
                    file_id,
                    messages: Vec::new(),
                }));
                self.contexts.last_mut().unwrap()
            }
        }
    }

    pub fn print_diagnostics(&self) {
        let mut config = Config::default();
        config = config.with_label_attach(LabelAttach::End);
        config = config.with_index_type(IndexType::Byte);

        for context in self.contexts.iter() {
            for message in context.messages.iter() {
                let severity = ReportKind::from(&message.severity);
                let file_id = message.location.file_id;
                let path = self.get_file_path(file_id).unwrap();
                let message_range = message.location.offset_range();
                let loc_tuple = (path.to_str().unwrap(), message_range);

                let mut builder = Report::build(severity, loc_tuple);

                builder = builder.with_config(config);
                builder = builder.with_message(message.title.clone());

                let mut colors = ColorGenerator::new();
                let cycled_colors = vec![
                    colors.next(),
                    colors.next(),
                    colors.next(),
                    colors.next(),
                    colors.next(),
                ]
                .into_iter()
                .cycle();
                let known_colors = vec![Color::from(&message.severity)].into_iter();
                let mut colors = known_colors.chain(cycled_colors);

                for label in &message.labels {
                    let label_range = label.location.offset_range();
                    let loc_tuple = (path.to_str().unwrap(), label_range);

                    let mut report_label = Label::new(loc_tuple);
                    if let Some(label) = &label.label {
                        report_label = report_label.with_message(label.clone());
                    }
                    report_label = report_label.with_color(colors.next().unwrap());

                    builder = builder.with_label(report_label);
                }

                for note in &message.notes {
                    builder = builder.with_note(note.clone());
                }

                let report = builder.finish();
                let source = self.get_file_source(file_id).unwrap();
                report
                    .print((path.to_str().unwrap(), Source::from(source)))
                    .unwrap();
                println!();
            }
        }
    }

    fn get_file_path(&self, file_id: FileId) -> Option<&PathBuf> {
        let source_file = self.get_file(file_id);
        match source_file {
            Some(source_file) => Some(&source_file.path),
            None => None,
        }
    }

    fn get_file_source(&self, file_id: FileId) -> Option<&str> {
        let source_file = self.get_file(file_id);
        match source_file {
            Some(source_file) => Some(&source_file.source),
            None => None,
        }
    }

    pub fn has_errors(&self) -> bool {
        for context in self.contexts.iter() {
            if context.has_errors() {
                return true;
            }
        }
        false
    }
}

impl DiagnosticContext {
    pub fn add_message(&mut self, message: DiagnosticMessage) {
        self.messages.push(message);
    }

    fn compose_impl(
        &mut self,
        severity: DiagnosticSeverity,
        title: &str,
        location: &DiagnosticLocation,
        labels: Vec<(Option<&str>, DiagnosticLocation)>,
        notes: Vec<&str>,
    ) {
        let mut labels: Vec<DiagnosticLabel> = labels
            .iter()
            .map(|(label, location)| DiagnosticLabel {
                label: label.map(str::to_string),
                location: location.clone(),
            })
            .collect();

        let notes = notes.iter().map(|note| note.to_string()).collect();

        // if no labels are given, use the message location as a label
        if labels.is_empty() {
            labels.push(DiagnosticLabel {
                label: None,
                location: location.clone(),
            });
        }

        self.add_message(DiagnosticMessage {
            severity,
            title: title.to_string(),
            location: location.clone(),
            labels,
            notes,
        });
    }

    pub fn error(
        &mut self,
        title: &str,
        location: &DiagnosticLocation,
        labels: Vec<(Option<&str>, DiagnosticLocation)>,
        notes: Vec<&str>,
    ) {
        self.compose_impl(DiagnosticSeverity::Error, title, location, labels, notes);
    }

    pub fn warning(
        &mut self,
        title: &str,
        location: &DiagnosticLocation,
        labels: Vec<(Option<&str>, DiagnosticLocation)>,
        notes: Vec<&str>,
    ) {
        self.compose_impl(DiagnosticSeverity::Warning, title, location, labels, notes);
    }

    pub fn advice(
        &mut self,
        title: &str,
        location: &DiagnosticLocation,
        labels: Vec<(Option<&str>, DiagnosticLocation)>,
        notes: Vec<&str>,
    ) {
        self.compose_impl(DiagnosticSeverity::Advice, title, location, labels, notes);
    }

    pub fn has_errors(&self) -> bool {
        for message in self.messages.iter() {
            if let DiagnosticSeverity::Error = message.severity {
                return true;
            }
        }
        false
    }
}
