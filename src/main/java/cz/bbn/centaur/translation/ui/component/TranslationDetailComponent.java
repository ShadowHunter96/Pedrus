package cz.bbn.cerberus.translation.ui.component;

import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.translation.dto.TranslationDto;

public class TranslationDetailComponent extends FormLayout {

    private final TranslationDto dto;
    private final Binder<TranslationDto> binder;

    public TranslationDetailComponent(TranslationDto dto) {
        this.dto = dto;
        binder = new Binder<>();
        binder.setBean(dto);
        setWidthFull();
        initComponent();
    }

    private void initComponent() {

        TextField id = new TextField(Transl.get("Id"));
        id.setValue("" + dto.getId());
        id.setReadOnly(true);
        if (dto.getId() != null) {
            this.add(id);
        }

        TextField lang = new TextField(Transl.get("Language"));
        lang.setReadOnly(true);
        binder.forField(lang).bind(TranslationDto::getLang, TranslationDto::setLang);
        this.add(lang);

        TextField key = new TextField(Transl.get("Key"));
        key.setReadOnly(true);
        binder.forField(key).bind(TranslationDto::getKey, TranslationDto::setKey);
        this.add(key);

        TextField value = new TextField((Transl.get("Value")));
        binder.forField(value).bind(TranslationDto::getValue, TranslationDto::setValue);
        this.add(value);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);

    }

    public TranslationDto getDto() {
        return binder.getBean();
    }
}
