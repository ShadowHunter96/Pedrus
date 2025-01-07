package cz.bbn.cerberus.commons.component.ui.dialog;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.dependency.CssImport;
import com.vaadin.flow.component.dialog.Dialog;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Footer;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.html.H4;
import com.vaadin.flow.component.html.Header;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.ConfirmAction;
import cz.bbn.cerberus.translation.Transl;

import java.util.Arrays;

@CssImport(themeFor = "vaadin-dialog-overlay", value = "./scrollable-dialog/scrollable-dialog.css")
@CssImport(value = "./scrollable-dialog/scrollable-dialog-renderer.css")
public class AppDialog extends Dialog {

    private final VerticalLayout content = new VerticalLayout();
    private final Footer footer = new Footer();
    private final HorizontalLayout header = new HorizontalLayout();

    private String title;
    private boolean showWarning = false;

    private void initDialog() {
        getElement().getThemeList().add("scrollable-dialog");

        this.setMaxWidth("95%");
        this.setMaxHeight("95%");

        header.setWidthFull();
        header.addComponentAtIndex(0, this.getHeading(title));
        this.add(new Header(header));

        content.setClassName("dialog-content");
        this.add(content);
        content.setAlignItems(FlexComponent.Alignment.CENTER);
        content.setJustifyContentMode(FlexComponent.JustifyContentMode.CENTER);
        content.addClassNames("scrollable-dialog-content");
        content.setSizeFull();

        this.add(footer);

        Div backgroundDiv = new Div();
        backgroundDiv.getElement().getStyle().set("width", "100%").set("height", "100%").set("position", "fixed")
                .set("opacity", "0").set("top", "0px").set("left", "0px").set("z-index", "-1");
        this.add(backgroundDiv);

        backgroundDiv.addClickListener(e -> {
            if (showWarning) {
                new ConfirmDialog(Transl.get("Are you sure? Data may be lost."), getWarningConfirmAction()).open();
            } else {
                super.setOpened(false);
            }
        });

    }

    public void addButtons(Button... buttons) {
        Arrays.stream(buttons).toList().forEach(button -> button.addClassName("dialog-button"));
        footer.add(buttons);
    }

    public void addCloseButton() {
        Button close = VaadinComponents.getCloseButton();
        close.addClickListener(buttonClickEvent -> this.close());
        addButtons(close);
    }

    public void addSubmitButton(Button submit) {
        addButtons(submit);
    }

    @Override
    public void open() {
        super.open();
        initDialog();
    }

    public void setContent(Component component) {
        component.getElement().getStyle().set("padding", "2px");
        content.add(component);
    }

    public void setTextAsContent(String text) {
        setContent(new H4(text));
    }

    public void setContent(Component... components) {
        Arrays.stream(components).toList().forEach(content::add);
    }

    private Component getHeading(String headingText) {
        H3 heading = new H3(headingText);
        heading.addClassName("dialog-title");
        return heading;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public VerticalLayout getContent() {
        return content;
    }

    @Override
    public void close() {
        if (showWarning) {
            new ConfirmDialog(Transl.get("Are you sure? Data may be lost."), getWarningConfirmAction()).open();
        } else {
            super.setOpened(false);
        }
    }

    public void showWarning(boolean showWarning) {
        this.showWarning = showWarning;
    }

    private ConfirmAction getWarningConfirmAction() {
        return () -> super.setOpened(false);
    }

}
