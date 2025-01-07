package cz.bbn.cerberus.commons.component.ui.field;


import com.vaadin.flow.component.Tag;
import com.vaadin.flow.component.dependency.JsModule;
import com.vaadin.flow.component.textfield.BigDecimalField;
import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;

@Tag("money-field")
@JsModule("./money/money-field.js")
@Slf4j
public class AppMoneyField extends BigDecimalField {

    public AppMoneyField() {
    }

    public AppMoneyField(String label) {
        super(label);
    }


    @Override
    public void setValue(BigDecimal value) {
        super.setValue(value);
        this.getElement().executeJs(" this.inputElement.value = Intl.NumberFormat('cs-CZ', " +
                "{style: 'currency', currency: 'CZK', currencyDisplay: 'code'}).format(this.value.replace(',', '.'))" +
                ".replace('CZK', '');");
    }


}
