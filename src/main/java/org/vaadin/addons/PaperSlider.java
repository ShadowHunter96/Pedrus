package org.vaadin.addons;

import com.vaadin.flow.component.HasValue;
import com.vaadin.flow.component.customfield.CustomField;
import com.vaadin.flow.component.html.Input;
import com.vaadin.flow.shared.Registration;



public class PaperSlider extends CustomField<Integer>
{
	PaperSliderComponent component;

	private Integer oldValue;

	public PaperSlider(Integer defaultValue) {
		this(defaultValue, 0, 100);
	}

	public PaperSlider(Integer defaultValue, Integer min, Integer max) {
		super(defaultValue);
		setWidthFull();
		component = new PaperSliderComponent(defaultValue, min, max);
		add(component);

		this.oldValue = defaultValue;
	}

	public void setMin(Integer min) {
		component.setMin(min);
	}

	public void setMax(Integer max) {
		component.setMax(max);
	}

	public void showValueWhenSliding(){
		component.showValueWhenSliding();
	}

	public void setValue(Integer value) {
		component.setValue(value);
	}

	public void setDisabled(boolean disabled) {
		component.setDisabled(disabled);
	}

	public void hideValueWhenSliding(){
		component.hideValueWhenSliding();
	}

	@Override
	protected Integer generateModelValue() {
		return getValue();
	}

	@Override
	protected void setPresentationValue(Integer integer) {
		component.setValue(integer);
	}

	@Override
	public Integer getValue(){
		return component.getValue();
	}

	public void setStep(Integer stepSize){
		component.setStep(stepSize);
	}

	@Override
	public Registration addValueChangeListener(ValueChangeListener<? super ComponentValueChangeEvent<CustomField<Integer>, Integer>> listener) {
		ValueChangeListener<ComponentValueChangeEvent<Input, Integer>> proxyListener = new ValueChangeListener<>() {
			@Override
			public void valueChanged(ComponentValueChangeEvent<Input, Integer> inputStringComponentValueChangeEvent) {
				HasValue<?, Integer> oldHasValue = inputStringComponentValueChangeEvent.getHasValue();
				HasValue<?, Integer> newHasValue = new CustomField<>() {
					Integer value;
					@Override
					protected Integer generateModelValue() {
						return value;
					}

					@Override
					protected void setPresentationValue(Integer integer) {
						this.value = integer;
					}
				};
				newHasValue.setValue(oldHasValue.getValue());
				newHasValue.setReadOnly(oldHasValue.isReadOnly());
			    listener.valueChanged(new ComponentValueChangeEvent<>(PaperSlider.this, newHasValue, oldValue, false));
				oldValue = newHasValue.getValue();
			}
		};

		return component.addValueChangeListener(proxyListener);
	}

	@Override
	public void setReadOnly(boolean b) {
		component.setReadOnly(b);
	}

	@Override
	public boolean isReadOnly() {
		return component.isReadOnly();
	}

	@Override
	public void setRequiredIndicatorVisible(boolean b) {
		component.setRequiredIndicatorVisible(b);
	}

	@Override
	public boolean isRequiredIndicatorVisible() {
		return component.isRequiredIndicatorVisible();
	}
}
