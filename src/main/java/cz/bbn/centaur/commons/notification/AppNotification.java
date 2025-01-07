package cz.bbn.cerberus.commons.notification;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.Text;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.notification.Notification;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;

public class AppNotification extends Notification {

    private static final long serialVersionUID = 1;

    private static final String ICON_SIZE = "32px";

    public AppNotification(String text, VaadinIcon icon) {
        this.setPosition(Position.TOP_CENTER);

        VerticalLayout content = this.getContentLayout();

        content.addClickListener(verticalLayoutClickEvent -> close());

        content.add(this.getCloseButton());

        HorizontalLayout iconText = new HorizontalLayout();
        iconText.setSizeFull();
        iconText.setAlignItems(FlexComponent.Alignment.CENTER);
        iconText.setJustifyContentMode(FlexComponent.JustifyContentMode.CENTER);


        Icon ico = new Icon(icon);
        ico.setSize("2em");
        Div icoDiv = new Div(ico);
        icoDiv.setWidth(ICON_SIZE);
        icoDiv.setHeight(ICON_SIZE);
        iconText.add(icoDiv);


        iconText.add(new Div(new Text(text)));

        content.add(iconText);

        this.add(content);

    }

    private VerticalLayout getContentLayout() {
        VerticalLayout content = new VerticalLayout();
        content.addClickListener(verticalLayoutClickEvent -> close());

        content.setSizeFull();
        content.setMargin(false);
        content.setPadding(false);
        content.setSpacing(false);

        content.getStyle().set("padding-bottom", "5px");

        return content;
    }

    private Component getCloseButton() {
        Icon close = new Icon(VaadinIcon.CLOSE);
        close.setSize("10px");
        close.getElement().getClassList().add("close-cross");


        HorizontalLayout closeLayout = new HorizontalLayout();

        closeLayout.getStyle().set("position", "relative").set("right", "-12px");

        closeLayout.setWidthFull();
        closeLayout.setAlignItems(FlexComponent.Alignment.END);
        closeLayout.setJustifyContentMode(FlexComponent.JustifyContentMode.END);

        closeLayout.setMargin(false);
        closeLayout.setPadding(false);
        closeLayout.setSpacing(false);


        closeLayout.add(close);
        return closeLayout;
    }

    protected void setNotificationDuration(int duration) {
        if (duration != 0) {
            this.setDuration(duration);
        }
    }

}
