package cz.bbn.cerberus.technology.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.technology.dto.TechnologyFilterDto;
import cz.bbn.cerberus.translation.Transl;


public class TechnologyFilterComponent extends FormLayout {

    private final Button search;

    private TextField id;
    private TextField name;
    private Checkbox showDeleted;

    public TechnologyFilterComponent(Button search) {
        this.search = search;
        initComponent();
    }

    private void initComponent() {
        id = new TextField(Transl.get("Id"));
        this.add(id);

        name = new TextField(Transl.get("Name"));
        this.add(name);

        showDeleted = new Checkbox(Transl.get("Show deleted"));
        showDeleted.setValue(false);
        if (SecurityUtils.hasPermission(Permission.TECHNOLOGY_SHOW_DELETED)) {
            this.add(showDeleted);
        }

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public TechnologyFilterDto getTechnologyFilterDto() {
        TechnologyFilterDto technologyFilterDto = new TechnologyFilterDto();
        technologyFilterDto.setName(name.getValue());
        technologyFilterDto.setId(id.getValue());
        technologyFilterDto.setShowDeleted(showDeleted.getValue());
        return technologyFilterDto;
    }
}
