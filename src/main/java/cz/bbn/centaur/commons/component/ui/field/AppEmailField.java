package cz.bbn.cerberus.commons.component.ui.field;

import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.data.binder.Setter;
import com.vaadin.flow.function.ValueProvider;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.validator.EmailValidator;
import cz.bbn.cerberus.commons.validator.EmptyValidator;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.StringUtils;

public class AppEmailField<BEAN> extends TextField {

    public AppEmailField(String label) {
        super(label);
        initComponent();
    }

    private void initComponent(){
        this.setPrefixComponent(VaadinIcon.ENVELOPE.create());
        this.setMaxLength(255);
    }

    public void bind(Binder<BEAN> binder, ValueProvider<BEAN, String> var1, Setter<BEAN, String> var2){
        bind(binder, var1, var2, true);
    }

    public void bind(Binder<BEAN> binder, ValueProvider<BEAN, String> var1, Setter<BEAN, String> var2, boolean required){
        if(required){
            binder.forField(this)
                    .withValidator(new EmptyValidator(Transl.get(TextValues.CANNOT_BE_EMPTY)))
                    .withValidator(new EmailValidator())
                    .bind(var1, var2);
        }else {
            binder.forField(this)
                    .withValidator(new EmailValidator())
                    .bind(var1, var2);
        }
    }
    @Override
    public String getValue() {
        String value = super.getValue();
        return value != null && (value.equalsIgnoreCase("@") || value.isEmpty()) ? null : value;
    }

    @Override
    public void setValue(String value) {
        if(StringUtils.isEmpty(value)){
            value = "@";
        }
        super.setValue(value);
    }
}
