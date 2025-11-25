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

use crate::charly::utils::diagnostics::DiagnosticContext;

#[track_caller]
pub fn validate_expected_diagnostics(
    context: &DiagnosticContext,
    expected_diagnostics: &[&str],
) {
    for diagnostic_message in &context.messages {
        let was_expected = expected_diagnostics
            .iter()
            .find(|message| diagnostic_message.title.eq(*message));
        assert!(
            was_expected.is_some(),
            "Unexpected diagnostic message: {}",
            diagnostic_message.title
        );
    }

    for expected_message in expected_diagnostics {
        let was_expected = &context
            .messages
            .iter()
            .find(|message| expected_message.eq(&message.title));
        assert!(
            was_expected.is_some(),
            "Expected diagnostic message not found: {}",
            expected_message
        );
    }
}
