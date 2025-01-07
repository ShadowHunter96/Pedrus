package cz.bbn.cerberus.commons.component.ui;

import com.vaadin.flow.component.Component;
import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;

import java.util.ArrayList;
import java.util.List;

public class AppAdvancedFilter extends FormLayout {

    private final List<Component> advancedComponentList = new ArrayList<>();
    private final List<Component> basicComponentList = new ArrayList<>();
    private final Button showFilter = VaadinComponents.getShowFilterButton("Show advanced filter");
    private final Button hideFilter = VaadinComponents.getHideFilterButton("Hide advanced filter");

    private final Button search;

    public AppAdvancedFilter(Button search) {
        this.search = search;
        init();
    }

    private void init() {
        hideFilter.setVisible(false);

        showFilter.addClickListener(e -> {
            advancedComponentList.forEach(component -> component.setVisible(true));
            showFilter.setVisible(false);
            hideFilter.setVisible(true);
        });

        hideFilter.addClickListener(e -> {
            advancedComponentList.forEach(component -> {
                if (component instanceof HasValue) {
                    if(component instanceof Checkbox) {
                        ((HasValue) component).setValue(false);
                    }else if(component instanceof TextField) {
                        ((HasValue) component).setValue("");
                    }else {
                        ((HasValue) component).setValue(null);
                    }
                }
                component.setVisible(false);
            });
            showFilter.setVisible(true);
            hideFilter.setVisible(false);
        });
    }

    public void addToAdvancedFilter(Component... components) {
        advancedComponentList.addAll(List.of(components));
    }

    public void addToBasicFilter(Component... components) {
        basicComponentList.addAll(List.of(components));
    }

    public void initFilter() {
        basicComponentList.forEach(this::add);
        advancedComponentList.forEach(component -> {
            component.setVisible(false);
            this.add(component);
        });
        this.add(search);
        if(!advancedComponentList.isEmpty()){
            this.add(showFilter, hideFilter);
        }
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

}
