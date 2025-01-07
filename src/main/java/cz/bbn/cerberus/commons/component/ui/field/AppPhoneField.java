package cz.bbn.cerberus.commons.component.ui.field;


import org.vaadin.addons.componentfactory.MaskedTextField;
import org.vaadin.addons.componentfactory.MaskedTextFieldOption;

public class AppPhoneField extends MaskedTextField {

    public AppPhoneField(String label) {
        setLabel(label);
        initComponent();
    }

    private void initComponent() {
        this.setMaskOptions(new MaskedTextFieldOption("mask", "000 000 000"));
    }
}
