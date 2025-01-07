package cz.bbn.cerberus.dph.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.dph.dto.DphFilterDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;


public class DphFilterComponent extends FormLayout {

    private final Button search;

    private TextField id;
    private TextField name;

    private Checkbox showNotAllowed;

    public DphFilterComponent(Button search) {
        this.search = search;
        initComponent();
    }

    private void initComponent() {
        id = new TextField(Transl.get("Id"));
        this.add(id);

        name = new TextField(Transl.get("Name"));
        this.add(name);

        showNotAllowed = new Checkbox(Transl.get("Show not allowed"));
        showNotAllowed.setValue(false);
        if (SecurityUtils.hasPermission(Permission.DPH_SHOW_NOT_ALLOWED)) {
            this.add(showNotAllowed);
        }

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public DphFilterDto getDphFilterDto() {
        DphFilterDto dphFilterDto = new DphFilterDto();
        dphFilterDto.setName(name.getValue());
        dphFilterDto.setId(id.getValue());
        dphFilterDto.setShowNotAllowed(showNotAllowed.getValue());
        return dphFilterDto;
    }
}
