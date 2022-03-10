//! Let your users split regions of your application and organize layout dynamically.
//!
//! [![Pane grid - Iced](https://thumbs.gfycat.com/MixedFlatJellyfish-small.gif)](https://gfycat.com/mixedflatjellyfish)
//!
//! # Example
//! The [`pane_grid` example] showcases how to use a [`PaneGrid`] with resizing,
//! drag and drop, and hotkey support.
//!
//! [`pane_grid` example]: https://github.com/iced-rs/iced/tree/0.3/examples/pane_grid
mod content;
mod title_bar;

pub use content::Content;
pub use title_bar::TitleBar;

pub use iced_native::widget::pane_grid::{
    Axis, Configuration, Direction, DragEvent, Node, Pane, ResizeEvent, Split,
};

use crate::overlay;
use crate::widget::tree::{self, Tree};
use crate::{Element, Widget};

use iced_native::event::{self, Event};
use iced_native::layout;
use iced_native::mouse;
use iced_native::renderer;
use iced_native::touch;
use iced_native::widget::pane_grid::state;
use iced_native::{
    Clipboard, Color, Layout, Length, Point, Rectangle, Shell, Size, Vector,
};

pub use iced_style::pane_grid::{Line, StyleSheet};

use std::collections::HashMap;

/// A collection of panes distributed using either vertical or horizontal splits
/// to completely fill the space available.
///
/// [![Pane grid - Iced](https://thumbs.gfycat.com/FrailFreshAiredaleterrier-small.gif)](https://gfycat.com/frailfreshairedaleterrier)
///
/// This distribution of space is common in tiling window managers (like
/// [`awesome`](https://awesomewm.org/), [`i3`](https://i3wm.org/), or even
/// [`tmux`](https://github.com/tmux/tmux)).
///
/// A [`PaneGrid`] supports:
///
/// * Vertical and horizontal splits
/// * Tracking of the last active pane
/// * Mouse-based resizing
/// * Drag and drop to reorganize panes
/// * Hotkey support
/// * Configurable modifier keys
/// * [`State`] API to perform actions programmatically (`split`, `swap`, `resize`, etc.)
///
/// ## Example
///
/// ```
/// # use iced_pure::widget::{pane_grid, text};
/// #
/// # type PaneGrid<'a, Message> =
/// #     iced_pure::widget::PaneGrid<'a, Message, iced_native::renderer::Null>;
/// #
/// enum PaneState {
///     SomePane,
///     AnotherKindOfPane,
/// }
///
/// enum Message {
///     PaneDragged(pane_grid::DragEvent),
///     PaneResized(pane_grid::ResizeEvent),
/// }
///
/// let (mut state, _) = pane_grid::State::new(PaneState::SomePane);
///
/// let pane_grid =
///     PaneGrid::new(&state, |pane, state| {
///         pane_grid::Content::new(match state {
///             PaneState::SomePane => text("This is some pane"),
///             PaneState::AnotherKindOfPane => text("This is another kind of pane"),
///         })
///     })
///     .on_drag(Message::PaneDragged)
///     .on_resize(10, Message::PaneResized);
/// ```
#[allow(missing_debug_implementations)]
pub struct PaneGrid<'a, Message, Renderer> {
    state: &'a state::Internal,
    elements: Vec<(Pane, Content<'a, Message, Renderer>)>,
    width: Length,
    height: Length,
    spacing: u16,
    on_click: Option<Box<dyn Fn(Pane) -> Message + 'a>>,
    on_drag: Option<Box<dyn Fn(DragEvent) -> Message + 'a>>,
    on_resize: Option<(u16, Box<dyn Fn(ResizeEvent) -> Message + 'a>)>,
    style_sheet: Box<dyn StyleSheet + 'a>,
}

#[derive(Debug)]
pub struct State<T> {
    panes: HashMap<Pane, T>,
    internal: state::Internal,
}

impl<'a, Message, Renderer> PaneGrid<'a, Message, Renderer>
where
    Renderer: iced_native::Renderer,
{
    /// Creates a [`PaneGrid`] with the given [`State`] and view function.
    ///
    /// The view function will be called to display each [`Pane`] present in the
    /// [`State`].
    pub fn new<T>(
        state: &'a State<T>,
        view: impl Fn(Pane, &'a T) -> Content<'a, Message, Renderer>,
    ) -> Self {
        let elements = {
            state
                .panes
                .iter()
                .map(|(pane, pane_state)| (*pane, view(*pane, pane_state)))
                .collect()
        };

        Self {
            elements,
            state: state.internal,
            width: Length::Fill,
            height: Length::Fill,
            spacing: 0,
            on_click: None,
            on_drag: None,
            on_resize: None,
            style_sheet: Default::default(),
        }
    }

    /// Sets the width of the [`PaneGrid`].
    pub fn width(mut self, width: Length) -> Self {
        self.width = width;
        self
    }

    /// Sets the height of the [`PaneGrid`].
    pub fn height(mut self, height: Length) -> Self {
        self.height = height;
        self
    }

    /// Sets the spacing _between_ the panes of the [`PaneGrid`].
    pub fn spacing(mut self, units: u16) -> Self {
        self.spacing = units;
        self
    }

    /// Sets the message that will be produced when a [`Pane`] of the
    /// [`PaneGrid`] is clicked.
    pub fn on_click<F>(mut self, f: F) -> Self
    where
        F: 'a + Fn(Pane) -> Message,
    {
        self.on_click = Some(Box::new(f));
        self
    }

    /// Enables the drag and drop interactions of the [`PaneGrid`], which will
    /// use the provided function to produce messages.
    pub fn on_drag<F>(mut self, f: F) -> Self
    where
        F: 'a + Fn(DragEvent) -> Message,
    {
        self.on_drag = Some(Box::new(f));
        self
    }

    /// Enables the resize interactions of the [`PaneGrid`], which will
    /// use the provided function to produce messages.
    ///
    /// The `leeway` describes the amount of space around a split that can be
    /// used to grab it.
    ///
    /// The grabbable area of a split will have a length of `spacing + leeway`,
    /// properly centered. In other words, a length of
    /// `(spacing + leeway) / 2.0` on either side of the split line.
    pub fn on_resize<F>(mut self, leeway: u16, f: F) -> Self
    where
        F: 'a + Fn(ResizeEvent) -> Message,
    {
        self.on_resize = Some((leeway, Box::new(f)));
        self
    }

    /// Sets the style of the [`PaneGrid`].
    pub fn style(mut self, style: impl Into<Box<dyn StyleSheet + 'a>>) -> Self {
        self.style_sheet = style.into();
        self
    }
}

impl<'a, Message, Renderer> Widget<Message, Renderer>
    for PaneGrid<'a, Message, Renderer>
where
    Renderer: iced_native::Renderer,
{
    fn width(&self) -> Length {
        self.width
    }

    fn height(&self) -> Length {
        self.height
    }

    fn layout(
        &self,
        renderer: &Renderer,
        limits: &layout::Limits,
    ) -> layout::Node {
        let limits = limits.width(self.width).height(self.height);
        let size = limits.resolve(Size::ZERO);

        let regions = self.state.pane_regions(f32::from(self.spacing), size);

        let children = self
            .elements
            .iter()
            .filter_map(|(pane, element)| {
                let region = regions.get(pane)?;
                let size = Size::new(region.width, region.height);

                let mut node =
                    element.layout(renderer, &layout::Limits::new(size, size));

                node.move_to(Point::new(region.x, region.y));

                Some(node)
            })
            .collect();

        layout::Node::with_children(size, children)
    }

    fn on_event(
        &mut self,
        tree: &mut Tree,
        event: Event,
        layout: Layout<'_>,
        cursor_position: Point,
        renderer: &Renderer,
        clipboard: &mut dyn Clipboard,
        shell: &mut Shell<'_, Message>,
    ) -> event::Status {
        let action = tree.state.downcast_mut::<state::Action>();
        let mut event_status = pane_grid::update(
            action,
            event,
            layout,
            cursor_position,
            renderer,
            clipboard,
            shell,
        );

        let picked_pane = action.picked_pane().map(|(pane, _)| pane);

        self.elements
            .iter_mut()
            .zip(&mut tree.children)
            .zip(layout.children())
            .map(|(((pane, content), tree), layout)| {
                let is_picked = picked_pane == Some(*pane);

                content.on_event(
                    tree,
                    event.clone(),
                    layout,
                    cursor_position,
                    renderer,
                    clipboard,
                    shell,
                    is_picked,
                )
            })
            .fold(event_status, event::Status::merge)
    }

    fn mouse_interaction(
        &self,
        tree: &Tree,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
        renderer: &Renderer,
    ) -> mouse::Interaction {
        if self.state.picked_pane().is_some() {
            return mouse::Interaction::Grab;
        }

        let resize_axis =
            self.state.picked_split().map(|(_, axis)| axis).or_else(|| {
                self.on_resize.as_ref().and_then(|(leeway, _)| {
                    let bounds = layout.bounds();

                    let splits = self
                        .state
                        .split_regions(f32::from(self.spacing), bounds.size());

                    let relative_cursor = Point::new(
                        cursor_position.x - bounds.x,
                        cursor_position.y - bounds.y,
                    );

                    hovered_split(
                        splits.iter(),
                        f32::from(self.spacing + leeway),
                        relative_cursor,
                    )
                    .map(|(_, axis, _)| axis)
                })
            });

        if let Some(resize_axis) = resize_axis {
            return match resize_axis {
                Axis::Horizontal => mouse::Interaction::ResizingVertically,
                Axis::Vertical => mouse::Interaction::ResizingHorizontally,
            };
        }

        self.elements
            .iter()
            .zip(&tree.children)
            .zip(layout.children())
            .map(|(((_pane, content), tree), layout)| {
                content.mouse_interaction(
                    tree,
                    layout,
                    cursor_position,
                    viewport,
                    renderer,
                )
            })
            .max()
            .unwrap_or_default()
    }

    fn draw(
        &self,
        tree: &Tree,
        renderer: &mut Renderer,
        style: &renderer::Style,
        layout: Layout<'_>,
        cursor_position: Point,
        viewport: &Rectangle,
    ) {
        let picked_pane = self.state.picked_pane();

        let picked_split = self
            .state
            .picked_split()
            .and_then(|(split, axis)| {
                let bounds = layout.bounds();

                let splits = self
                    .state
                    .split_regions(f32::from(self.spacing), bounds.size());

                let (_axis, region, ratio) = splits.get(&split)?;

                let region = axis.split_line_bounds(
                    *region,
                    *ratio,
                    f32::from(self.spacing),
                );

                Some((axis, region + Vector::new(bounds.x, bounds.y), true))
            })
            .or_else(|| match self.on_resize {
                Some((leeway, _)) => {
                    let bounds = layout.bounds();

                    let relative_cursor = Point::new(
                        cursor_position.x - bounds.x,
                        cursor_position.y - bounds.y,
                    );

                    let splits = self
                        .state
                        .split_regions(f32::from(self.spacing), bounds.size());

                    let (_split, axis, region) = hovered_split(
                        splits.iter(),
                        f32::from(self.spacing + leeway),
                        relative_cursor,
                    )?;

                    Some((
                        axis,
                        region + Vector::new(bounds.x, bounds.y),
                        false,
                    ))
                }
                None => None,
            });

        let pane_cursor_position = if picked_pane.is_some() {
            // TODO: Remove once cursor availability is encoded in the type
            // system
            Point::new(-1.0, -1.0)
        } else {
            cursor_position
        };

        for (((id, pane), tree), layout) in self
            .elements
            .iter()
            .zip(&tree.children)
            .zip(layout.children())
        {
            match picked_pane {
                Some((dragging, origin)) if *id == dragging => {
                    let bounds = layout.bounds();

                    renderer.with_translation(
                        cursor_position
                            - Point::new(
                                bounds.x + origin.x,
                                bounds.y + origin.y,
                            ),
                        |renderer| {
                            renderer.with_layer(bounds, |renderer| {
                                pane.draw(
                                    tree,
                                    renderer,
                                    style,
                                    layout,
                                    pane_cursor_position,
                                    viewport,
                                );
                            });
                        },
                    );
                }
                _ => {
                    pane.draw(
                        tree,
                        renderer,
                        style,
                        layout,
                        pane_cursor_position,
                        viewport,
                    );
                }
            }
        }

        if let Some((axis, split_region, is_picked)) = picked_split {
            let highlight = if is_picked {
                self.style_sheet.picked_split()
            } else {
                self.style_sheet.hovered_split()
            };

            if let Some(highlight) = highlight {
                renderer.fill_quad(
                    renderer::Quad {
                        bounds: match axis {
                            Axis::Horizontal => Rectangle {
                                x: split_region.x,
                                y: (split_region.y
                                    + (split_region.height - highlight.width)
                                        / 2.0)
                                    .round(),
                                width: split_region.width,
                                height: highlight.width,
                            },
                            Axis::Vertical => Rectangle {
                                x: (split_region.x
                                    + (split_region.width - highlight.width)
                                        / 2.0)
                                    .round(),
                                y: split_region.y,
                                width: highlight.width,
                                height: split_region.height,
                            },
                        },
                        border_radius: 0.0,
                        border_width: 0.0,
                        border_color: Color::TRANSPARENT,
                    },
                    highlight.color,
                );
            }
        }
    }

    fn overlay<'b>(
        &'b self,
        tree: &'b mut Tree,
        layout: Layout<'_>,
        renderer: &Renderer,
    ) -> Option<overlay::Element<'_, Message, Renderer>> {
        self.elements
            .iter_mut()
            .zip(&mut tree.children)
            .zip(layout.children())
            .filter_map(|(((_, pane), tree), layout)| {
                pane.overlay(tree, layout, renderer)
            })
            .next()
    }
}

impl<'a, Message, Renderer> From<PaneGrid<'a, Message, Renderer>>
    for Element<'a, Message, Renderer>
where
    Renderer: 'a + iced_native::Renderer,
    Message: 'a,
{
    fn from(
        pane_grid: PaneGrid<'a, Message, Renderer>,
    ) -> Element<'a, Message, Renderer> {
        Element::new(pane_grid)
    }
}

/*
 * Helpers
 */
fn hovered_split<'a>(
    splits: impl Iterator<Item = (&'a Split, &'a (Axis, Rectangle, f32))>,
    spacing: f32,
    cursor_position: Point,
) -> Option<(Split, Axis, Rectangle)> {
    splits
        .filter_map(|(split, (axis, region, ratio))| {
            let bounds =
                axis.split_line_bounds(*region, *ratio, f32::from(spacing));

            if bounds.contains(cursor_position) {
                Some((*split, *axis, bounds))
            } else {
                None
            }
        })
        .next()
}
