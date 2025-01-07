package cz.bbn.cerberus.commons.component.ui.slider;

import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.NumberField;
import org.vaadin.addons.PaperSlider;

public class AppSlider extends VerticalLayout {

    private final String text;
    private final Integer initValue;

    private PaperSlider paperSlider;
    private String labelPrefix = "";

    public AppSlider(String text, Integer initValue, String labelPrefix) {
        this.text = text;
        this.initValue = initValue;
        this.labelPrefix = labelPrefix;
        initComponent();
    }

    private void initComponent() {
        Label label = new Label(text.concat(": ").concat(String.valueOf(initValue)).concat(" ").concat(labelPrefix));
        this.setAlignItems(Alignment.CENTER);

        paperSlider = new PaperSlider(initValue);
        paperSlider.addValueChangeListener(event -> label.setText(text.concat(": ")
                .concat(String.valueOf(event.getValue())).concat(" ").concat(labelPrefix)));
        paperSlider.setDisabled(false);
        paperSlider.setWidth("15em");
        paperSlider.setRequiredIndicatorVisible(true);
        paperSlider.showValueWhenSliding();
        paperSlider.setStep(1);
        this.add(paperSlider, label);
        this.setHeight("1.7em");
        label.setClassName("project-slider-label");
    }

    public void setStep(Integer step) {
        paperSlider.setStep(step);
    }

    public void setLabelPrefix(String labelPrefix) {
        this.labelPrefix = labelPrefix;
    }

    public PaperSlider getPaperSlider() {
        return paperSlider;
    }
}
