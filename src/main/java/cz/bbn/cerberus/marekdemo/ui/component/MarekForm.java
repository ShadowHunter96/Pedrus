package cz.bbn.cerberus.marekdemo.ui.component;

import com.vaadin.flow.component.Key;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.marekdemo.dto.MarekDTO;
import cz.bbn.cerberus.translation.Transl;

/**
 * Created by marek.vu on 05.10.2023.
 */
public class MarekForm extends FormLayout {
    TextField name = new TextField("Name");
    TextField description = new TextField("Description");

    Button save = new Button("Save");
    Button delete = new Button("Delete");
    Button close = new Button("Cancel");
    //BeanValidationBinder<MarekDTO>binder = new BeanValidationBinder<>(MarekDTO.class);
    Binder<MarekDTO> binder = new Binder<>();

    private final SaveAction<MarekDTO> saveAction;


    public MarekForm(SaveAction<MarekDTO> marekSaveAction) {
        this.saveAction = marekSaveAction;
        addClassName("marek-form");
        //binder.bindInstanceFields(this);
        binder.setBean(new MarekDTO());

        binder.forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).bind(MarekDTO::getName, MarekDTO::setName);
        binder.forField(description).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(MarekDTO::getDescription, MarekDTO::setDescription);

        add(name,
                description,
                createButtonsLayout());


    }

    private HorizontalLayout createButtonsLayout() {
        save.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        delete.addThemeVariants(ButtonVariant.LUMO_ERROR);
        close.addThemeVariants(ButtonVariant.LUMO_TERTIARY);
        save.addClickShortcut(Key.ENTER);
        close.addClickShortcut(Key.ESCAPE);

        save.addClickListener(event -> validateAndSave());
//  ;
        return new HorizontalLayout(save, delete, close);
    }

    // saving method
    private void validateAndSave() {
        if (binder.validate().isOk()) {

            saveAction.saveItem(binder.getBean(), null);
            binder.setBean(new MarekDTO());
        }
    }
    //deleting entity


    public MarekDTO getMarekDTO() {
        return binder.getBean();
    }

}