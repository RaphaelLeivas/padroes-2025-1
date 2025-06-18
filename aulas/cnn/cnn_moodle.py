import tensorflow as tf
from tensorflow.keras.datasets import mnist
from tensorflow.keras.utils import to_categorical
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Conv2D, MaxPooling2D, Flatten, Dense

import matplotlib.pyplot as plt
import time

# Carregar os dados
(x_train, y_train), (x_test, y_test) = mnist.load_data()

# Pré-processamento
x_train = x_train.reshape(-1, 28, 28, 1).astype("float32") / 255.0
x_test  = x_test.reshape(-1, 28, 28, 1).astype("float32") / 255.0
y_train = to_categorical(y_train, 10)
y_test  = to_categorical(y_test, 10)

# Definir a CNN
f_list = [1, 2, 4, 8, 16, 32, 64, 128]
# f_list = [1, 2]
acc_array = []
time_array = []
pool_list = [2]
dense_list = [64]

for d in dense_list:
    start_time = time.time()
    model = Sequential([
        Conv2D(64, kernel_size=(5, 5), activation='relu', input_shape=(28, 28, 1)),
        MaxPooling2D(pool_size=(2, 2)),
        Conv2D(64, kernel_size=(5, 5), activation='relu'),
        MaxPooling2D(pool_size=(2, 2)),
        Conv2D(64, kernel_size=(5, 5), activation='relu'),
        MaxPooling2D(pool_size=(2, 2)),
        Conv2D(64, kernel_size=(5, 5), activation='relu'),
        MaxPooling2D(pool_size=(2, 2)),
        Conv2D(64, kernel_size=(5, 5), activation='relu'),
        MaxPooling2D(pool_size=(2, 2)),
        Flatten(),
        Dense(d, activation='relu'),
        Dense(10, activation='softmax')
    ])

    # Compilar o modelo
    model.compile(optimizer='adam',
                loss='categorical_crossentropy',
                metrics=['accuracy'])

    # Treinar o modelo
    model.fit(x_train, y_train,
            epochs=2,
            batch_size=128,
            validation_split=0.2)

    # Avaliar no conjunto de teste
    test_loss, test_accuracy = model.evaluate(x_test, y_test)
    print(f"Loss: {test_loss:.4f}, Accuracy: {test_accuracy:.4f}")
    acc_array.append(test_accuracy)
    time_array.append(time.time() - start_time)

fig, ax1 = plt.subplots()
ax1.set_xlabel('Número de neurônios - Camada Dense')
ax1.set_ylabel('Acurácia', color='red')
ax1.plot(dense_list, acc_array, color='red', marker='o', label='Acurácia')
ax1.tick_params(axis='y', labelcolor='red')

ax2 = ax1.twinx()

ax2.set_ylabel('Tempo de Execução (s)', color='blue')
ax2.plot(dense_list, time_array, color='blue', marker='s', label='Tempo de Execução')
ax2.tick_params(axis='y', labelcolor='blue')

plt.grid(True)

plt.show()