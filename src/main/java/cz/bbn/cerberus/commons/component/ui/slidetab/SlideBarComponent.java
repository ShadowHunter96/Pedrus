package cz.bbn.cerberus.commons.component.ui.slidetab;

import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.dom.ClassList;
import cz.bbn.cerberus.commons.CssVariables;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class SlideBarComponent extends VerticalLayout {

    private final List<SlideTabItem> slideTabItemList;

    private final Div backgroundDiv = new Div();

    private List<Div> buttonList;

    public SlideBarComponent(List<SlideTabItem> slideTabItemList) {
        this.slideTabItemList = slideTabItemList;
        getElement().getStyle().set("position", "relative");
        setPadding(false);
        setWidthFull();
        setHeight("20px");
        addButtonsLayout();
        addVerticalLayout();
        addBackgroundLayout();
    }

    private void addButtonsLayout() {

        int width = 9 * slideTabItemList.size();

        int marginOffset = 0;
        if (width != 0) {
            marginOffset = width / 2;
        }

        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.getElement().getStyle().set("left", "calc(50% - " + marginOffset + "em)");
        horizontalLayout.addClassName("top-button-bar");
        horizontalLayout.setWidth(width + "em");
        horizontalLayout.setSpacing(false);
        horizontalLayout.setJustifyContentMode(JustifyContentMode.CENTER);
        buttonList = new ArrayList<>();
        for (int i = 0; i < slideTabItemList.size(); i++) {
            Div button = new Div();
            button.addClassName("div-button");
            Div text = new Div();
            button.add(text);
            text.setText(slideTabItemList.get(i).getText());
            buttonList.add(button);
            if (i == 0) {
                button.getElement().getStyle().set("border-radius", "0em 0em 0em 1em");
            }
            if (i == (slideTabItemList.size() - 1)) {
                button.getElement().getStyle().set("border-radius", "0em 0em 1em 0em");
            }
            if (slideTabItemList.size() == 1) {
                button.getElement().getStyle().set("border-radius", "0em 0em 1em 1em");
            }
            if (slideTabItemList.get(i).getIndicator() != null) {
                text.add(slideTabItemList.get(i).getIndicator());
            }
        }
        int j = 0;
        for (SlideTabItem slideTabItem : slideTabItemList) {
            Div button = buttonList.get(j);
            horizontalLayout.add(button);
            if (j < (slideTabItemList.size() - 1)) {
                Div whiteLine = new Div();
                whiteLine.getElement().getStyle().set("background-color", "white").set("width", "1px")
                        .set("height", "70%");
                horizontalLayout.add(whiteLine);
                horizontalLayout.setVerticalComponentAlignment(Alignment.CENTER, whiteLine);
            }
            button.addClickListener(e -> {
                for (SlideTabItem slideTabItem1 : slideTabItemList) {
                    if (!Objects.equals(slideTabItem1.getName(), slideTabItem.getName())) {
                        slideTabItem1.getComponent().setVisible(false);
                        ClassList classList = slideTabItem1.getComponent().getElement().getClassList();
                        classList.set(CssVariables.SLIDE_IN_CLASS.getValue(), false);
                        classList.set(CssVariables.SLIDE_OUT_CLASS.getValue(), true);
                        slideTabItem1.setVisible(false);
                    }
                }
                for (Div buttonDiv : buttonList) {
                    buttonDiv.getElement().getStyle().set("background-color", "");
                }
                ClassList classList = slideTabItem.getComponent().getElement().getClassList();
                if (slideTabItem.isVisible()) {
                    classList.set(CssVariables.SLIDE_IN_CLASS.getValue(), false);
                    classList.set(CssVariables.SLIDE_OUT_CLASS.getValue(), true);
                    button.getElement().getStyle().set("background-color", "");
                } else {
                    classList.set(CssVariables.SLIDE_IN_CLASS.getValue(), true);
                    classList.set(CssVariables.SLIDE_OUT_CLASS.getValue(), false);
                    button.getElement().getStyle().set("background-color", "#bbbcbd");
                }
                slideTabItem.getComponent().setVisible(!slideTabItem.isVisible());
                slideTabItem.setVisible(!slideTabItem.isVisible());
                backgroundDiv.setVisible(slideTabItem.isVisible());
            });
            slideTabItem.getComponent().setVisible(false);
            j++;
        }
        if (!slideTabItemList.isEmpty()) {
            add(horizontalLayout);
        }
    }

    private void addVerticalLayout() {
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setPadding(false);
        verticalLayout.setWidth("50%");
        verticalLayout.setAlignItems(Alignment.CENTER);
        verticalLayout.addClassName("top-card-layout");
        for (SlideTabItem slideTabItem : slideTabItemList) {
            verticalLayout.add(slideTabItem.getComponent());
        }
        add(verticalLayout);
    }

    private void addBackgroundLayout() {
        backgroundDiv.setHeightFull();
        backgroundDiv.setWidthFull();
        backgroundDiv.getElement().getStyle().set("position", "fixed").set("z-index", "40").set("opacity", "0");
        backgroundDiv.addClickListener(e -> {
            for (SlideTabItem slideTabItem : slideTabItemList) {
                slideTabItem.getComponent().setVisible(false);
                ClassList classList = slideTabItem.getComponent().getElement().getClassList();
                classList.set(CssVariables.SLIDE_IN_CLASS.getValue(), false);
                classList.set(CssVariables.SLIDE_OUT_CLASS.getValue(), true);
                slideTabItem.setVisible(false);
            }
            for (Div buttonDiv : buttonList) {
                buttonDiv.getElement().getStyle().set("background-color", "");
                backgroundDiv.setVisible(false);
            }
        });
        backgroundDiv.setVisible(false);
        add(backgroundDiv);
    }
}
