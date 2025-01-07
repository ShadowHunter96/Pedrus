package cz.bbn.cerberus.commons;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.dom.ClassList;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.slidetab.CountIntIndicator;
import cz.bbn.cerberus.translation.Transl;
import org.vaadin.erik.SlideMode;
import org.vaadin.erik.SlideTab;
import org.vaadin.erik.SlideTabBuilder;

public class AppSlideTab extends SlideTab {

    private final String text;
    private final SlideMode slideMode;

    private CountIntIndicator countIntIndicator;

    public AppSlideTab(SlideTabBuilder builder, String text, SlideMode slideMode) {
        super(builder);
        this.text = text;
        this.slideMode = slideMode;
        initComponent();
    }

    public AppSlideTab(SlideTabBuilder builder, String text, SlideMode slideMode, CountIntIndicator countIntIndicator) {
        super(builder);
        this.text = text;
        this.slideMode = slideMode;
        this.countIntIndicator = countIntIndicator;
        initComponent();
    }

    private void initComponent() {
        Button buttonExpand = VaadinComponents.getButton(Transl.get(text), VaadinIcon.ANGLE_DOUBLE_UP.create());
        buttonExpand.getElement().setAttribute(TextValues.TITLE, Transl.get("Show".concat(" ").concat(text)));
        buttonExpand.setClassName("slide-tab-button");
        buttonExpand.addClickListener(buttonClickEvent -> getElement().getStyle().set("z-index", "2"));

        Button buttonCollapse = VaadinComponents.getButton(Transl.get(text), VaadinIcon.ANGLE_DOUBLE_DOWN.create());
        buttonCollapse.getElement().setAttribute(TextValues.TITLE, Transl.get("Close".concat(" ").concat(text)));
        buttonCollapse.setClassName("slide-tab-button");
        buttonCollapse.addClickListener(buttonClickEvent -> getElement().getStyle().set("z-index", "1"));

        if (slideMode != null && (slideMode == SlideMode.TOP || slideMode == SlideMode.LEFT)) {
            buttonExpand.setIcon(VaadinIcon.ANGLE_DOUBLE_DOWN.create());
            buttonCollapse.setIcon(VaadinIcon.ANGLE_DOUBLE_UP.create());
        }

        HorizontalLayout expandHorizontalLayout = new HorizontalLayout();
        expandHorizontalLayout.add(buttonExpand);
        setExpandComponent(expandHorizontalLayout);

        HorizontalLayout collapseHorizontalLayout = new HorizontalLayout();
        collapseHorizontalLayout.add(buttonCollapse);
        setCollapseComponent(collapseHorizontalLayout);

        if (countIntIndicator != null) {
            ClassList classList = countIntIndicator.getElement().getClassList();
            classList.set("new-slide-tab-count-span", false);
            classList.set("slide-tab-count-span", true);
            CountIntIndicator secondIndicator = new CountIntIndicator(countIntIndicator.getCount());
            ClassList secondClassList = secondIndicator.getElement().getClassList();
            secondClassList.set("new-slide-tab-count-span", false);
            secondClassList.set("slide-tab-count-span", true);
            expandHorizontalLayout.add(countIntIndicator);
            collapseHorizontalLayout.add(secondIndicator);
        }
    }
}
