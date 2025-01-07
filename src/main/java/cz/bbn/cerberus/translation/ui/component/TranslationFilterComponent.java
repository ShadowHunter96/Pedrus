package cz.bbn.cerberus.translation.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.translation.dto.TranslationFilterDto;

import java.util.List;

public class TranslationFilterComponent extends FormLayout {

    private final List<String> langSet;
    private final Button searchButton;

    private IntegerField id;
    private ComboBox<String> lang;
    private TextField key;
    private TextField value;
    private Checkbox showEmpty;

    public TranslationFilterComponent(Button searchButton, List<String> langSet) {
        this.langSet = langSet;
        this.searchButton = searchButton;
        initFilter();
    }

    private void initFilter() {
        setWidthFull();

        id = new IntegerField(Transl.get("Id"));
        add(id);

        lang = new ComboBox<>(Transl.get("Language"));
        lang.setItems(langSet);
        add(lang);

        key = new TextField(Transl.get("Key"));
        add(key);

        value = new TextField(Transl.get("Value"));
        add(value);

        showEmpty = new Checkbox(Transl.get("Show empty values"));
        showEmpty.setValue(false);
        add(showEmpty);

        add(searchButton);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public TranslationFilterDto getTranslationFilterDto() {
        TranslationFilterDto filterDto = new TranslationFilterDto();
        if (id.getValue() != null) {
            filterDto.setId(Long.valueOf(id.getValue()));
        }
        filterDto.setLang(lang.getValue());
        filterDto.setKey(key.getValue());
        filterDto.setValue(value.getValue());
        filterDto.setShowEmpty(showEmpty.getValue());
        return filterDto;
    }
}
