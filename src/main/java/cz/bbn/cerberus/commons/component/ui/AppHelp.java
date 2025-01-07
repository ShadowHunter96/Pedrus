package cz.bbn.cerberus.commons.component.ui;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasComponents;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.html.H3;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.translation.Transl;

public class AppHelp extends Icon {

    private final boolean showDialog;
    private final String title;
    private AppTooltip tooltip;

    public AppHelp(HasComponents mainComponent, String title, boolean showDialog) {
        super(VaadinIcon.INFO_CIRCLE);
        this.showDialog = showDialog;
        this.title = title;
        initComponent();
        mainComponent.add(tooltip);
    }

    public AppHelp(HasComponents mainComponent, String title) {
        super(VaadinIcon.INFO_CIRCLE);
        this.showDialog = false;
        this.title = title;
        initComponent();
        mainComponent.add(tooltip);
    }


    public AppHelp(HasComponents mainComponent, String title, int margin) {
        super(VaadinIcon.INFO_CIRCLE);
        this.showDialog = false;
        this.title = title;
        initComponent();
        tooltip.getElement().getStyle().set("margin", margin + "em");
        mainComponent.add(tooltip);
    }

    private void initComponent() {
        tooltip = new AppTooltip();
    }

    public void setContent(Component... components) {
        if (showDialog) {
            this.addClickListener(iconClickEvent -> {
                AppDialog appDialog = new AppDialog();
                appDialog.setContent(components);
                appDialog.setTitle(title);

                Button close = VaadinComponents.getCloseButton();
                appDialog.addButtons(close);
                close.addClickListener(buttonClickEvent -> appDialog.close());
                appDialog.open();

            });
            tooltip.add(new Label(Transl.get("Help is too big. Please click on icon for showing full content")));
            this.setClassName("cursor-pointer");
        } else {
            if (title != null) {
                tooltip.add(new H3(title));
            }
            tooltip.add(components);
        }
        tooltip.attachToComponent(this);
    }

    public AppTooltip getAppTooltip() {
        return tooltip;
    }
}
